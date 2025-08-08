const scanning = @import("scanning.zig");
const std = @import("std");

// TODO: implement better grammar from 6.1
// expression     → equality ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary
//                | primary ;
// primary        → NUMBER | STRING | "true" | "false" | "nil"
//                | "(" expression ")" ;

pub const ParsingError = error{
    UnexpectedToken,
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

pub const Expression = union(enum) {
    literal: union(enum) {
        number: f64,
        string: []u8,
        true,
        false,
        nil,
    },
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
    tokens: []scanning.Token,
    position: usize,

    pub fn new(tokens: []scanning.Token) AstParser {
        return .{ .tokens = tokens, .position = 0 };
    }

    // Tries to peek at the token at the position of our parser. Returns null if we are at the end of the list.
    fn tryPeek(self: *AstParser) ?scanning.Token {
        if (self.position < self.tokens.len - 1) {
            return self.tokens[self.position];
        } else {
            return null;
        }
    }

    fn advance(self: *AstParser) void {
        self.position += 1;
    }

    // The way this AST parser works is somewhat simple.
    // Each rule, described by the table above is a function.
    // The function mutates the state of the parser, moving the position forward
    // to the token immediately after the expression it returns.

    // Returns the root of the AST.
    pub fn parse(self: *AstParser, allocator: std.mem.Allocator) (std.mem.Allocator.Error || ParsingError)!*Expression {
        return try self.expressionRule(allocator);
    }

    fn expressionRule(self: *AstParser, allocator: std.mem.Allocator) (std.mem.Allocator.Error || ParsingError)!*Expression {
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

    fn equalityRule(self: *AstParser, allocator: std.mem.Allocator) (std.mem.Allocator.Error || ParsingError)!*Expression {
        const matches = &[_]TokenToBinaryExpr{ .{ .key = .bangEqual, .value = .notEquality }, .{ .key = .equalEqual, .value = .equality } };
        return self.binaryRule(allocator, matches, comparisonRule);
    }
    fn comparisonRule(self: *AstParser, allocator: std.mem.Allocator) (std.mem.Allocator.Error || ParsingError)!*Expression {
        const matches = &[_]TokenToBinaryExpr{ .{ .key = .greater, .value = .greater }, .{ .key = .greaterEqual, .value = .greaterEqual }, .{ .key = .less, .value = .less }, .{ .key = .lessEqual, .value = .lessEqual } };
        return self.binaryRule(allocator, matches, termRule);
    }
    fn termRule(self: *AstParser, allocator: std.mem.Allocator) (std.mem.Allocator.Error || ParsingError)!*Expression {
        const matches = &[_]TokenToBinaryExpr{ .{ .key = .plus, .value = .add }, .{ .key = .minus, .value = .subtract } };
        return self.binaryRule(allocator, matches, factorRule);
    }
    fn factorRule(self: *AstParser, allocator: std.mem.Allocator) (std.mem.Allocator.Error || ParsingError)!*Expression {
        const matches = &[_]TokenToBinaryExpr{ .{ .key = .star, .value = .multiply }, .{ .key = .slash, .value = .divide } };
        return self.binaryRule(allocator, matches, unaryRule);
    }
    fn unaryRule(self: *AstParser, allocator: std.mem.Allocator) (std.mem.Allocator.Error || ParsingError)!*Expression {
        const opToken = self.tokens[self.position];

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
        const token = self.tokens[self.position];
        self.advance();

        if (token.tokenType == .leftParen) {
            const expr = try self.expressionRule(allocator);

            // the token will be the token following expr
            const current = self.tokens[self.position];
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

pub fn printExpression(expr: *Expression, out: std.io.AnyWriter) !void {
    switch (expr.*) {
        .literal => |l| switch (l) {
            .number => |num| try out.print("{d}", .{num}),
            .string => |str| try out.print("{s}", .{str}),
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
