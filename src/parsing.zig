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
        return .{ .tokens = tokens };
    }

    // Recursive descent parser helpers.
    fn next(self: *AstParser) ?scanning.Token {
        if (self.position < self.tokens.len) {
            self.position += 1;
            return self.tokens[self.position];
        } else {
            return null;
        }
    }

    // The way this AST parser works is somewhat simple.
    // Each rule, described by the table above is a function.
    // The function mutates the state of the parser, moving the position forward
    // to the token immediately after the expression it returns.

    // Returns the root of the AST.
    pub fn parse(self: *AstParser, allocator: std.mem.Allocator) !*Expression {
        return try self.expressionRule(allocator);
    }

    fn expressionRule(self: *AstParser, allocator: std.mem.Allocator) !*Expression {
        return try self.equalityRule(allocator);
    }

    // it's funny the only thing that can fail here (previousRule is meant to be another call to binaryRule)
    fn binaryRule(self: *AstParser, allocator: std.mem.Allocator, matches: []TokenToBinaryExpr, previousRule: fn (*AstParser, std.mem.Allocator) anyerror!*Expression) !*Expression {
        const expression = try previousRule(&self, allocator);

        while (self.next()) |token| {
            const operation = matchTokenToExprOrNull(token.tokenType, matches) orelse break;

            const right = try previousRule(&self, allocator);

            const newRoot = try allocator.create(Expression);

            newRoot.binary = .{ .left = expression, .right = right, .operation = operation };
        }
    }
    fn equalityRule(self: *AstParser, allocator: std.mem.Allocator) !*Expression {
        return self.binaryRule(allocator, .{ .{ .key = .bangEqual, .value = .notEquality }, .{ .key = .equalEqual, .value = .equality } }, AstParser.comparisonRule);
    }
    fn comparisonRule(self: *AstParser, allocator: std.mem.Allocator) !*Expression {
        return self.binaryRule(allocator, .{ .{ .key = .greater, .value = .greater }, .{ .key = .greaterEqual, .value = .greaterEqual }, .{ .key = .less, .value = .less }, .{ .key = .lessEqual, .value = .lessEqual } }, AstParser.comparisonRule);
    }
    fn termRule(self: *AstParser, allocator: std.mem.Allocator) !*Expression {
        return self.binaryRule(allocator, .{ .{ .key = .plus, .value = .add }, .{ .key = .minus, .value = .subtract } }, AstParser.comparisonRule);
    }
    fn factorRule(self: *AstParser, allocator: std.mem.Allocator) !*Expression {
        return self.binaryRule(allocator, .{ .{ .key = .star, .value = .multiply }, .{ .key = .slash, .value = .divide } }, AstParser.comparisonRule);
    }
    fn unaryRule(self: *AstParser, allocator: std.mem.Allocator) !*Expression {
        const opToken = self.tokens[self.position];

        const operation = switch (opToken.tokenType) {
            .bang => .negate,
            .minus => .negateBool,
            else => return try self.primaryRule(allocator),
        };

        const right = try self.unaryRule(allocator);

        const newRoot = try allocator.create(Expression);
        newRoot.unary = .{ .operation = operation, .expr = right };
    }
    fn primaryRule(self: *AstParser, allocator: std.mem.Allocator) !*Expression {
        const token = self.tokens[self.position];

        const primary = try allocator.create(Expression);
        primary.* = lit: switch (token.tokenType) {
            .number => {
                const literal = Expression{ .literal = .{ .number = 0.5 } };
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
