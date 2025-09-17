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

pub const Evaluator = struct {
    runtime: runtime.Runtime,

    pub fn init(rt: runtime.Runtime) Evaluator {
        return .{ .runtime = rt };
    }

    pub fn deinit(self: *Evaluator) void {
        self.runtime.deinit();
    }

    pub fn evaluateBlock(self: *Evaluator, allocator: Allocator, block: *parsing.Block) !Result {
        for (block.contents) |c| _ = switch (c) {
            .stmt => |s| try self.evaluateNode(allocator, s.expr),
            .block => |b| try self.evaluateBlock(allocator, b),
        };
        return .{ .literal = .nil };
    }

    pub fn evaluateNode(self: *Evaluator, allocator: Allocator, expression: *parsing.Expression) (anyerror || Allocator.Error || runtime.RuntimeError || EvaluationError)!Result {
        switch (expression.*) {
            .assignment => |a| {
                const val = try self.evaluateNode(allocator, a.value);
                if (val == .literal) {
                    try self.runtime.set(a.name, val.literal);
                }
                return .{ .literal = .nil };
            },
            .binary => |b| return self.evaluateBinary(
                allocator,
                b.operation,
                try self.evaluateNode(allocator, b.left),
                try self.evaluateNode(allocator, b.right),
            ),
            .unary => |u| return self.evaluateUnary(u.operation, try self.evaluateNode(allocator, u.expr)),
            .literal => |l| return .{ .literal = l },
            .grouping => |u| return self.evaluateNode(allocator, u.expr),
            .functionCall => |f| {
                if (std.mem.eql(u8, f.name, parsing.VERYBADPRINTFUNCTIONNAME)) {
                    // hack
                    const stderr = std.io.getStdErr().writer().any();
                    _ = try stderr.write("PRINTING: ");
                    for (f.args) |a| {
                        const result = try self.evaluateNode(allocator, a);
                        try printResult(result, stderr);
                        _ = try stderr.write(" ");
                    }
                    _ = try stderr.write("\n");
                }
                return .{ .literal = .nil };
            },
            .declaration => |d| return self.evaluateDeclaration(allocator, d.name, d.value),
            .variable => |v| return .{ .literal = try self.getNameAsLiteral(v.name) },
        }
    }

    // We need the allocator for string concatenation (+)
    pub fn evaluateBinary(self: *Evaluator, allocator: Allocator, op: parsing.BinaryExprType, left: Result, right: Result) !Result {
        const leftLit = switch (left) {
            .handle => |h| try self.getHandleAsLiteral(h),
            .literal => |l| l,
        };
        const rightLit = switch (right) {
            .handle => |h| try self.getHandleAsLiteral(h),
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
            .modulo => {
                if (leftLit == .number and rightLit == .number) {
                    return .{ .literal = .{ .number = @mod(leftLit.number, rightLit.number) } };
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
            .bOr => {
                if (leftLit == .bool and rightLit == .bool) {
                    return .{ .literal = .{ .bool = leftLit.bool or rightLit.bool } };
                } else {
                    return EvaluationError.NoOperationForOperands;
                }
            },
            .bAnd => {
                if (leftLit == .bool and rightLit == .bool) {
                    return .{ .literal = .{ .bool = leftLit.bool and rightLit.bool } };
                } else {
                    return EvaluationError.NoOperationForOperands;
                }
            },
        }
    }

    pub fn evaluateUnary(self: *Evaluator, op: parsing.UnaryExprType, right: Result) !Result {
        const operand = switch (right) {
            .handle => |h| try self.getHandleAsLiteral(h),
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

    pub fn evaluateDeclaration(self: *Evaluator, allocator: Allocator, name: []u8, value: ?*parsing.Expression) !Result {
        const v: ?parsing.Literal = val: {
            if (value != null) {
                const res = try self.evaluateNode(allocator, value orelse unreachable);
                switch (res) {
                    .literal => |l| {
                        break :val l;
                    },
                    .handle => {
                        break :val null;
                    },
                }
            } else {
                break :val null;
            }
        };
        const handle = try self.runtime.declare(name, v);
        return .{ .handle = handle };
    }

    pub fn getHandleAsLiteral(self: *Evaluator, handle: runtime.VarHandle) !parsing.Literal {
        return try self.runtime.get(handle);
    }

    pub fn getNameAsLiteral(self: *Evaluator, name: []u8) !parsing.Literal {
        return try self.runtime.getByName(name);
    }
};
pub fn printResult(result: Result, out: std.io.AnyWriter) !void {
    switch (result) {
        .literal => |lit| {
            const expr = parsing.Expression{ .literal = lit };
            try parsing.printExpression(@constCast(&expr), out);
        },
        .handle => _ = try out.write("handle (WIP)"),
    }
}
