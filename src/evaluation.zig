const std = @import("std");
const parsing = @import("parsing.zig");
const runtime = @import("runtime.zig");
const Allocator = std.mem.Allocator;

pub const EvaluationError = error{
    NoOperationForOperands,
    IncompatibleTypesForOperands,
};

pub const Result = union(enum) {
    handle: runtime.VarHandle,
    literal: parsing.Literal,
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
        .functionCall => return evaluateLiteral(.nil),
        .declaration => return evaluateLiteral(.nil),
    }
}

// We need the allocator for string concatenation (+)
pub fn evaluateBinary(allocator: Allocator, op: parsing.BinaryExprType, left: Result, right: Result) !Result {
    const leftLit = switch (left) {
        .handle => .nil,
        .literal => |l| l,
    };
    const rightLit = switch (right) {
        .handle => .nil,
        .literal => |l| l,
    };
    switch (op) {
        .add => {
            if (leftLit == .number and rightLit == .number) {
                return .{ .literal = .{ .number = leftLit.number + rightLit.number } };
            } else if (leftLit == .string and rightLit == .string) {
                return .{ .literal = .{ .string = try std.mem.concat(allocator, u8, &[_][]const u8{ leftLit.string, rightLit.string }) } };
            } else {
                return EvaluationError.NoOperationForOperands;
            }
        },
        .subtract => {
            if (leftLit == .number and rightLit == .number) {
                return .{ .literal = .{ .number = leftLit.number - rightLit.number } };
            } else {
                return EvaluationError.NoOperationForOperands;
            }
        },
        .multiply => {
            if (leftLit == .number and rightLit == .number) {
                return .{ .literal = .{ .number = leftLit.number * rightLit.number } };
            } else {
                return EvaluationError.NoOperationForOperands;
            }
        },
        .divide => {
            if (leftLit == .number and rightLit == .number) {
                return .{ .literal = .{ .number = leftLit.number / rightLit.number } };
            } else {
                return EvaluationError.NoOperationForOperands;
            }
        },
        .greater => {
            if (leftLit == .number and rightLit == .number) {
                return .{ .literal = .{ .bool = leftLit.number > rightLit.number } };
            } else {
                return EvaluationError.NoOperationForOperands;
            }
        },
        .greaterEqual => {
            if (leftLit == .number and rightLit == .number) {
                return .{ .literal = .{ .bool = leftLit.number >= rightLit.number } };
            } else {
                return EvaluationError.NoOperationForOperands;
            }
        },
        .less => {
            if (leftLit == .number and rightLit == .number) {
                return .{ .literal = .{ .bool = leftLit.number < rightLit.number } };
            } else {
                return EvaluationError.NoOperationForOperands;
            }
        },
        .lessEqual => {
            if (leftLit == .number and rightLit == .number) {
                return .{ .literal = .{ .bool = leftLit.number <= rightLit.number } };
            } else {
                return EvaluationError.NoOperationForOperands;
            }
        },
        .equality => {
            if (leftLit == .number and rightLit == .number) {
                return .{ .literal = .{ .bool = leftLit.number == rightLit.number } };
            } else if (leftLit == .string and rightLit == .string) {
                return .{ .literal = .{ .bool = std.mem.eql(u8, leftLit.string, rightLit.string) } };
            } else {
                return EvaluationError.NoOperationForOperands;
            }
        },
        .notEquality => {
            if (leftLit == .number and rightLit == .number) {
                return .{ .literal = .{ .bool = leftLit.number != rightLit.number } };
            } else if (leftLit == .string and rightLit == .string) {
                return .{ .literal = .{ .bool = !std.mem.eql(u8, leftLit.string, rightLit.string) } };
            } else {
                return EvaluationError.NoOperationForOperands;
            }
        },
    }
}

pub fn evaluateUnary(op: parsing.UnaryExprType, right: Result) !Result {
    const operand = switch (right) {
        .handle => .nil,
        .literal => |l| l,
    };
    switch (operand) {
        .number => |num| switch (op) {
            .negate => return .{ .literal = .{ .number = -num } },
            else => return EvaluationError.NoOperationForOperands,
        },
        .bool => |b| switch (op) {
            .negateBool => return .{ .literal = .{ .bool = !b } },
            else => return EvaluationError.NoOperationForOperands,
        },
        else => return EvaluationError.NoOperationForOperands,
    }
}
pub inline fn evaluateLiteral(literal: parsing.Literal) Result {
    return .{ .literal = literal };
}
pub fn printResult(result: Result, out: std.io.AnyWriter) !void {
    switch (result) {
        .literal => |lit| {
            const expr = parsing.Expression{ .literal = lit };
            try parsing.printExpression(@constCast(&expr), out);
        },
        .handle => _ = try out.write("handle (WIP)"),
    }
}
