const std = @import("std");

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
