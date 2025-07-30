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

pub const Expression = union(enum) {
    literal: union(enum) {
        number: f64,
        string: []u8,
        true,
        false,
        nil,
    },
    unary: struct {
        operation: enum { negate, negateBool },
        expr: *Expression,
    },
    binary: struct {
        operation: enum {
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
        },
        left: *Expression,
        right: *Expression,
    },
    grouping: struct { expr: *Expression },
};

fn matchToken(target: scanning.TokenType, matches: []const scanning.TokenType) bool {
    for (matches) |t| {
        if (t == target) {
            return true;
        }
    }
    return false;
}

pub const AstParser = struct {
    tokens: []scanning.Token,
    position: usize,

    pub fn new(tokens: []scanning.Token) AstParser {
        return .{ .tokens = tokens };
    }

    // Recursive descent parser helpers.
    fn next(self: *AstParser) ?scanning.TokenType {
        if (self.position < self.tokens.len) {
            self.position += 1;
            return self.tokens[self.position].tokenType;
        } else {
            return null;
        }
    }

    // The way this AST parser works is somewhat simple.
    // Each rule, described by the table above is a function.
    // The function mutates the state of the parser, moving the position forward
    // to the token immediately after the expression it returns.

    fn expressionRule(self: *AstParser, allocator: std.mem.Allocator) !*Expression {
        return try self.equalityRule(allocator);
    }
    fn equalityRule(self: *AstParser, allocator: std.mem.Allocator) !*Expression {
        const expression = try self.comparisonRule(allocator);

        while (self.next()) |token| {
            if (!matchToken(token, .{ .bangEqual, .equalEqual })) {
                break;
            }

            const right = try self.comparsionRule(allocator);

            const newRoot = try allocator.create(Expression);
            newRoot.binary = .{ .left = expression, .right = right, .operation = switch (token) {
                .bang_equal => .notEquality,
                .equal_equal => .equality,
                else => .equality,
            } };
        }

        return expression;
    }
    fn comparisonRule(self: *AstParser, allocator: std.mem.Allocator) !*Expression {}
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
