const std = @import("std");

pub const Expression = union(enum) {
    literal: union(enum) {
        number: f32,
        string: []u8,
        true,
        false,
        nil,
    },
    unary: struct {
        operation: enum { Negate, NegateBool },
        expr: *Expression,
    },
    binary: struct {
        operation: enum {
            Equality,
            NotEquality,
            Greater,
            GreaterEqual,
            Less,
            LessEqual,
            Add,
            Subtract,
            Multiply,
            Divide,
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
            .true => try out.write("true"),
            .false => try out.write("false"),
            .nil => try out.write("nil"),
        },
        .unary => |u| {
            switch (u.operation) {
                .Negate => try out.write("(- "),
                .NegateBool => try out.write("(! "),
            }
            try printExpression(u.expr, out);
            try out.write(")");
        },
        .binary => |b| {
            switch (b.operation) {
                .Equality => try out.write("(== "),
                .NotEquality => try out.write("(!= "),
                .Greater => try out.write("(> "),
                .GreaterEqual => try out.write("(>= "),
                .Less => try out.write("(< "),
                .LessEqual => try out.write("(<= "),
                .Add => try out.write("(+ "),
                .Subtract => try out.write("(- "),
                .Multiply => try out.write("(* "),
                .Divide => try out.write("(/ "),
            }
            try printExpression(b.left, out);
            try printExpression(b.right, out);
            try out.write(")");
        },
        .grouping => |g| {
            try out.write("(group ");
            try printExpression(g.expr, out);
            try out.write(")");
        },
    }
}
