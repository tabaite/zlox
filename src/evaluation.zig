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
            switch (left) {
                .number => |leftNum| switch (right) {
                    .number => |rightNum| return .{ .number = leftNum + rightNum },
                    else => return EvaluationError.IncompatibleTypesForOperands,
                },
                .string => |leftStr| switch (right) {
                    .string => |rightStr| {
                        const buffer = try std.mem.concat(allocator, u8, &[_][]const u8{ leftStr, rightStr });
                        return .{ .string = buffer };
                    },
                    else => return EvaluationError.IncompatibleTypesForOperands,
                },
                else => return EvaluationError.NoOperationForOperands,
            }
        },
        .subtract => {
            switch (left) {
                .number => |leftNum| switch (right) {
                    .number => |rightNum| return .{ .number = leftNum - rightNum },
                    else => return EvaluationError.IncompatibleTypesForOperands,
                },
                else => return EvaluationError.NoOperationForOperands,
            }
        },
        .multiply => {
            switch (left) {
                .number => |leftNum| switch (right) {
                    .number => |rightNum| return .{ .number = leftNum - rightNum },
                    else => return EvaluationError.IncompatibleTypesForOperands,
                },
                else => return EvaluationError.NoOperationForOperands,
            }
        },
        .divide => {
            switch (left) {
                .number => |leftNum| switch (right) {
                    .number => |rightNum| return .{ .number = leftNum - rightNum },
                    else => return EvaluationError.IncompatibleTypesForOperands,
                },
                else => return EvaluationError.NoOperationForOperands,
            }
        },
        .greater => {
            switch (left) {
                .number => |leftNum| switch (right) {
                    .number => |rightNum| return .{ .bool = leftNum > rightNum },
                    else => return EvaluationError.IncompatibleTypesForOperands,
                },
                else => return EvaluationError.NoOperationForOperands,
            }
        },
        .greaterEqual => {
            switch (left) {
                .number => |leftNum| switch (right) {
                    .number => |rightNum| return .{ .bool = leftNum >= rightNum },
                    else => return EvaluationError.IncompatibleTypesForOperands,
                },
                else => return EvaluationError.NoOperationForOperands,
            }
        },
        .less => {
            switch (left) {
                .number => |leftNum| switch (right) {
                    .number => |rightNum| return .{ .bool = leftNum < rightNum },
                    else => return EvaluationError.IncompatibleTypesForOperands,
                },
                else => return EvaluationError.NoOperationForOperands,
            }
        },
        .lessEqual => {
            switch (left) {
                .number => |leftNum| switch (right) {
                    .number => |rightNum| return .{ .bool = leftNum < rightNum },
                    else => return EvaluationError.IncompatibleTypesForOperands,
                },
                else => return EvaluationError.NoOperationForOperands,
            }
        },
        .equality => {
            switch (left) {
                .number => |leftNum| switch (right) {
                    .number => |rightNum| return .{ .bool = leftNum == rightNum },
                    else => return EvaluationError.IncompatibleTypesForOperands,
                },
                .string => |leftStr| switch (right) {
                    .string => |rightStr| {
                        return .{ .bool = std.mem.eql(u8, leftStr, rightStr) };
                    },
                    else => return EvaluationError.IncompatibleTypesForOperands,
                },
                else => return EvaluationError.NoOperationForOperands,
            }
        },
        .notEquality => {
            switch (left) {
                .number => |leftNum| switch (right) {
                    .number => |rightNum| return .{ .bool = leftNum != rightNum },
                    else => return EvaluationError.IncompatibleTypesForOperands,
                },
                .string => |leftStr| switch (right) {
                    .string => |rightStr| {
                        return .{ .bool = !std.mem.eql(u8, leftStr, rightStr) };
                    },
                    else => return EvaluationError.IncompatibleTypesForOperands,
                },
                else => return EvaluationError.NoOperationForOperands,
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
