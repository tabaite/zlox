const scanning = @import("scanning.zig");
const std = @import("std");

// program        → ( statement )* EOF
// statement      → ( expression | call ) ";"
// call           → IDENTIFIER "(" ( ( expression "," )* expression ) ")"
// expression     → equality
// equality       → comparison ( ( "!=" | "==" ) comparison )*
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )*
// term           → factor ( ( "-" | "+" ) factor )*
// factor         → unary ( ( "/" | "*" ) unary )*
// unary          → ( "!" | "-" ) unary
//                | primary
// primary        → NUMBER | STRING | "true" | "false" | "nil"
//                | "(" expression ")"

pub const ParsingError = error{
    UnexpectedToken,
    ExpectedToken,
    ExpectedSemicolon,
    ExpectedClosingBrace,
};

pub const BinaryExprType = enum {
    equality,
    notEquality,
    greater,
    greaterEqual,
    less,
    lessEqual,
    add,
    subtract,
    multiply,
    divide,
};

pub const UnaryExprType = enum {
    negate,
    negateBool,
};

pub const Literal = union(enum) {
    number: f64,
    string: []u8,
    true,
    false,
    nil,
};

pub const Statement = struct {
    expr: *Expression,
    next: ?*Statement,
};

pub const Expression = union(enum) {
    functionCall: struct {
        name: []u8,
    },
    literal: Literal,
    unary: struct {
        operation: UnaryExprType,
        expr: *Expression,
    },
    binary: struct {
        operation: BinaryExprType,
        left: *Expression,
        right: *Expression,
    },
    grouping: struct { expr: *Expression },
};

const TokenToBinaryExpr = struct {
    key: scanning.TokenType,
    value: BinaryExprType,
};

fn matchTokenToExprOrNull(target: scanning.TokenType, matches: []const TokenToBinaryExpr) ?BinaryExprType {
    for (matches) |t| {
        if (t.key == target) {
            return t.value;
        }
    }
    return null;
}

pub const AstParser = struct {
    iter: *scanning.TokenIterator,
    lastToken: ?scanning.Token,

    pub fn new(iter: *scanning.TokenIterator) AstParser {
        return .{ .iter = iter, .lastToken = iter.next() };
    }

    // Tries to peek at the token at the position of our parser. Returns null if we are at the end of the list.
    pub fn tryPeek(self: *AstParser) ?scanning.Token {
        return self.lastToken;
    }

    fn advance(self: *AstParser) void {
        self.lastToken = self.iter.next();
    }

    // The way this AST parser works is somewhat simple.
    // Each rule, described by the table above is a function.
    // The function mutates the state of the parser, moving the position forward
    // to the token immediately after the expression it returns.

    // Returns the root of the AST.
    pub fn parse(self: *AstParser, allocator: std.mem.Allocator) !*Statement {
        return try self.programRule(allocator);
    }

    fn programRule(self: *AstParser, allocator: std.mem.Allocator) !*Statement {
        const root = try self.statementRule(allocator);
        var currentStatement = root;
        while (self.tryPeek()) |_| {
            const newStatement = try self.statementRule(allocator);
            currentStatement.next = newStatement;

            currentStatement = newStatement;
        }
        return root;
    }

    fn statementRule(self: *AstParser, allocator: std.mem.Allocator) !*Statement {
        const expression = try self.functionCallRule(allocator);
        const end = self.tryPeek() orelse scanning.Token{ .tokenType = .invalidChar, .source = null };
        if (end.tokenType != .semicolon) {
            return ParsingError.ExpectedSemicolon;
        }
        const statement = try allocator.create(Statement);
        statement.* = .{ .expr = expression, .next = null };
        self.advance();
        return statement;
    }

    fn functionCallRule(self: *AstParser, allocator: std.mem.Allocator) !*Expression {
        const name = self.tryPeek() orelse return self.expressionRule(allocator);
        if (name.tokenType != .identifier and name.tokenType != .kwPrint) {
            return self.expressionRule(allocator);
        }
        self.advance();
        const startParen = self.tryPeek() orelse return ParsingError.ExpectedToken;

        if (startParen.tokenType != .leftParen) {
            return self.expressionRule(allocator);
        }
        self.advance();

        while (self.tryPeek()) |t| {
            if (t.tokenType == .rightParen) {
                break;
            }
            _ = try self.functionCallRule(allocator);
            const seperator = self.tryPeek() orelse scanning.Token{ .tokenType = .invalidChar, .source = null };
            if (seperator.tokenType != .comma) {
                break;
            }
            self.advance();
        }

        const endParen = self.tryPeek() orelse return ParsingError.ExpectedClosingBrace;
        if (endParen.tokenType != .rightParen) {
            return ParsingError.ExpectedClosingBrace;
        }
        const expr = try allocator.create(Expression);
        const fnName = if (name.tokenType == .kwPrint) @constCast("(built-in) print") else name.source orelse @constCast("NULL TOKEN !!");
        expr.* = Expression{ .functionCall = .{ .name = fnName } };
        self.advance();
        return expr;
    }

    fn expressionRule(self: *AstParser, allocator: std.mem.Allocator) !*Expression {
        return try self.equalityRule(allocator);
    }

    // might be the most atrocious function body i've ever written
    fn binaryRule(self: *AstParser, allocator: std.mem.Allocator, matches: []const TokenToBinaryExpr, previousRule: fn (*AstParser, std.mem.Allocator) (std.mem.Allocator.Error || ParsingError)!*Expression) (std.mem.Allocator.Error || ParsingError)!*Expression {
        var expression = try previousRule(self, allocator);
        while (self.tryPeek()) |token| {
            const operation = matchTokenToExprOrNull(token.tokenType, matches) orelse break;

            self.advance();

            const right = try previousRule(self, allocator);

            const newRoot = try allocator.create(Expression);

            newRoot.* = Expression{ .binary = .{ .left = expression, .right = right, .operation = operation } };

            expression = newRoot;
        }
        return expression;
    }

    fn equalityRule(self: *AstParser, allocator: std.mem.Allocator) !*Expression {
        const matches = &[_]TokenToBinaryExpr{ .{ .key = .bangEqual, .value = .notEquality }, .{ .key = .equalEqual, .value = .equality } };
        return self.binaryRule(allocator, matches, comparisonRule);
    }
    fn comparisonRule(self: *AstParser, allocator: std.mem.Allocator) !*Expression {
        const matches = &[_]TokenToBinaryExpr{ .{ .key = .greater, .value = .greater }, .{ .key = .greaterEqual, .value = .greaterEqual }, .{ .key = .less, .value = .less }, .{ .key = .lessEqual, .value = .lessEqual } };
        return self.binaryRule(allocator, matches, termRule);
    }
    fn termRule(self: *AstParser, allocator: std.mem.Allocator) !*Expression {
        const matches = &[_]TokenToBinaryExpr{ .{ .key = .plus, .value = .add }, .{ .key = .minus, .value = .subtract } };
        return self.binaryRule(allocator, matches, factorRule);
    }
    fn factorRule(self: *AstParser, allocator: std.mem.Allocator) !*Expression {
        const matches = &[_]TokenToBinaryExpr{ .{ .key = .star, .value = .multiply }, .{ .key = .slash, .value = .divide } };
        return self.binaryRule(allocator, matches, unaryRule);
    }
    fn unaryRule(self: *AstParser, allocator: std.mem.Allocator) !*Expression {
        const opToken = self.tryPeek() orelse return error.ExpectedToken;

        const operation: UnaryExprType = switch (opToken.tokenType) {
            .bang => .negateBool,
            .minus => .negate,
            else => return try self.primaryRule(allocator),
        };

        self.advance();

        const right = try self.unaryRule(allocator);

        const newRoot = try allocator.create(Expression);
        newRoot.* = Expression{ .unary = .{ .operation = operation, .expr = right } };

        return newRoot;
    }
    fn primaryRule(self: *AstParser, allocator: std.mem.Allocator) (std.mem.Allocator.Error || ParsingError)!*Expression {
        const token = self.tryPeek() orelse return error.ExpectedToken;
        self.advance();

        if (token.tokenType == .leftParen) {
            const expr = try self.expressionRule(allocator);

            // the token will be the token following expr
            const current = self.tryPeek() orelse return error.ExpectedToken;
            if (current.tokenType != .rightParen) {
                return error.UnexpectedToken;
            } else {
                const group = try allocator.create(Expression);
                group.* = Expression{ .grouping = .{ .expr = expr } };
                self.advance();
                return group;
            }
        }

        const primary = try allocator.create(Expression);
        primary.* = lit: switch (token.tokenType) {
            .number => {
                const literal = Expression{ .literal = .{ .number = std.fmt.parseFloat(f64, token.source orelse "0") catch 0 } };
                break :lit literal;
            },
            .string => {
                const literal = Expression{ .literal = .{ .string = token.source orelse unreachable } };
                break :lit literal;
            },
            .kwNil => break :lit Expression{ .literal = .nil },
            .kwTrue => break :lit Expression{ .literal = .true },
            .kwFalse => break :lit Expression{ .literal = .false },
            // todo: expressions in parethesis
            else => return error.UnexpectedToken,
        };
        return primary;
    }
};

pub fn printStatements(stmt: *Statement, out: std.io.AnyWriter) !void {
    var current: ?*Statement = stmt;
    while (current != null) {
        const actual = current orelse unreachable;
        try printExpression(actual.expr, out);
        _ = try out.write("\n");
        current = actual.next;
    }
}

pub fn printExpression(expr: *Expression, out: std.io.AnyWriter) !void {
    switch (expr.*) {
        .functionCall => |f| try out.print("CALL \"{s}\"", .{f.name}),
        .literal => |l| switch (l) {
            .number => |num| try out.print("{d}", .{num}),
            .string => |str| try out.print("\"{s}\"", .{str}),
            .true => _ = try out.write("true"),
            .false => _ = try out.write("false"),
            .nil => _ = try out.write("nil"),
        },
        .unary => |u| {
            _ = switch (u.operation) {
                .negate => try out.write("(- "),
                .negateBool => try out.write("(! "),
            };
            try printExpression(u.expr, out);
            _ = try out.write(")");
        },
        .binary => |b| {
            _ = switch (b.operation) {
                .equality => try out.write("(== "),
                .notEquality => try out.write("(!= "),
                .greater => try out.write("(> "),
                .greaterEqual => try out.write("(>= "),
                .less => try out.write("(< "),
                .lessEqual => try out.write("(<= "),
                .add => try out.write("(+ "),
                .subtract => try out.write("(- "),
                .multiply => try out.write("(* "),
                .divide => try out.write("(/ "),
            };
            try printExpression(b.left, out);
            _ = try out.write(" ");
            try printExpression(b.right, out);
            _ = try out.write(")");
        },
        .grouping => |g| {
            _ = try out.write("(group ");
            try printExpression(g.expr, out);
            _ = try out.write(")");
        },
    }
}
