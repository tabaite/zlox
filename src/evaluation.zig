const std = @import("std");
const parsing = @import("parsing.zig");
const Allocator = std.mem.Allocator;

pub const EvaluationError = error{
    NoOperationForOperands,
    IncompatibleTypesForOperands,
};

pub const Types = enum {
    number,
    string,
    bool,
    nil,
};
pub const Result = union(Types) {
    number: f64,
    string: []u8,
    bool: bool,
    nil,
};

// TODO: wrap this into a struct. That way, we can "work around" Zig's primitive errors
// by having error information recoverable from the state.

pub fn evaluateNode(allocator: Allocator, expression: *parsing.Expression) !Result {
    switch (expression.*) {
        .binary => |b| return try evaluateBinary(
            allocator,
            b.operation,
            try evaluateNode(allocator, b.left),
            try evaluateNode(allocator, b.right),
        ),
        .unary => |u| return try evaluateUnary(u.operation, try evaluateNode(allocator, u.expr)),
        .literal => |l| return evaluateLiteral(l),
        .grouping => |u| return try evaluateNode(allocator, u.expr),
    }
}

// We need the allocator for string concatenation (+)
pub fn evaluateBinary(allocator: Allocator, op: parsing.BinaryExprType, left: Result, right: Result) !Result {
    switch (op) {
        .add => {
            if (left == .number and right == .number) {
                return .{ .number = left.number + right.number };
            } else if (left == .string and right == .string) {
                return .{ .string = try std.mem.concat(allocator, u8, &[_][]const u8{ left.string, right.string }) };
            } else {
                return EvaluationError.NoOperationForOperands;
            }
        },
        .subtract => {
            if (left == .number and right == .number) {
                return .{ .number = left.number - right.number };
            } else {
                return EvaluationError.NoOperationForOperands;
            }
        },
        .multiply => {
            if (left == .number and right == .number) {
                return .{ .number = left.number * right.number };
            } else {
                return EvaluationError.NoOperationForOperands;
            }
        },
        .divide => {
            if (left == .number and right == .number) {
                return .{ .number = left.number / right.number };
            } else {
                return EvaluationError.NoOperationForOperands;
            }
        },
        .greater => {
            if (left == .number and right == .number) {
                return .{ .bool = left.number > right.number };
            } else {
                return EvaluationError.NoOperationForOperands;
            }
        },
        .greaterEqual => {
            if (left == .number and right == .number) {
                return .{ .bool = left.number >= right.number };
            } else {
                return EvaluationError.NoOperationForOperands;
            }
        },
        .less => {
            if (left == .number and right == .number) {
                return .{ .bool = left.number < right.number };
            } else {
                return EvaluationError.NoOperationForOperands;
            }
        },
        .lessEqual => {
            if (left == .number and right == .number) {
                return .{ .bool = left.number <= right.number };
            } else {
                return EvaluationError.NoOperationForOperands;
            }
        },
        .equality => {
            if (left == .number and right == .number) {
                return .{ .bool = left.number == right.number };
            } else if (left == .string and right == .string) {
                return .{ .bool = std.mem.eql(u8, left.string, right.string) };
            } else {
                return EvaluationError.NoOperationForOperands;
            }
        },
        .notEquality => {
            if (left == .number and right == .number) {
                return .{ .bool = left.number != right.number };
            } else if (left == .string and right == .string) {
                return .{ .bool = !std.mem.eql(u8, left.string, right.string) };
            } else {
                return EvaluationError.NoOperationForOperands;
            }
        },
    }
}

pub fn evaluateUnary(op: parsing.UnaryExprType, right: Result) !Result {
    switch (right) {
        .number => |num| switch (op) {
            .negate => return .{ .number = -num },
            else => return EvaluationError.NoOperationForOperands,
        },
        .bool => |b| switch (op) {
            .negateBool => return .{ .bool = !b },
            else => return EvaluationError.NoOperationForOperands,
        },
        else => return EvaluationError.NoOperationForOperands,
    }
}
pub fn evaluateLiteral(literal: parsing.Literal) Result {
    switch (literal) {
        .nil => return .nil,
        .true => return .{ .bool = true },
        .false => return .{ .bool = false },
        .string => |str| return .{ .string = str },
        .number => |num| return .{ .number = num },
    }
}
pub fn printResult(result: Result, out: std.io.AnyWriter) !void {
    switch (result) {
        .bool => |b| try out.print("{s}", .{if (b) "true" else "false"}),
        .number => |n| try out.print("{d}", .{n}),
        .string => |str| try out.print("{s}", .{str}),
        .nil => _ = try out.write("nil"),
    }
}
