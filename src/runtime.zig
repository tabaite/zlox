// Any type value that is not the given built-in values is
// a user-defined class.

const std = @import("std");
const bytecode = @import("bytecode.zig");
const common = @import("common.zig");
const Allocator = std.mem.Allocator;

pub const RuntimeError = error{
    StackOverflow,
    OutOfStackBounds,
    UndeclaredVariableAccessed,
    VariableAlreadyDeclared,
};

pub const Operand = bytecode.RawOperand;

// A handle to a variable stored in the IRHandler.
pub const Handle = bytecode.Handle;

test "push varstack" {
    var debug = std.heap.DebugAllocator(.{}){};
    defer _ = debug.deinit();
    const gpa = debug.allocator();
    var stack = try VarStack.init(gpa, gpa);
    stack.pop();

    stack.deinit();
}

const Call = struct { returnPosition: usize, varStackHeight: usize };

// The usize counts how high the stack is in terms of variables (ABSOLUTE POSITION).
pub const CallStack = common.Stack(Call, 16777215);
pub const VarStack = common.Stack(Operand, 16777215);
pub const ArgBuffer = common.Stack(Operand, 128);

pub const Runtime = struct {
    stringAllocator: Allocator,
    variableStack: VarStack,
    callStack: CallStack,
    argBuffer: ArgBuffer,
    pub fn init(allocator: Allocator, stringAllocator: Allocator) !Runtime {
        return .{
            .stringAllocator = stringAllocator,
            .variableStack = try VarStack.init(allocator),
            .argBuffer = try ArgBuffer.init(allocator),
            .callStack = try CallStack.init(allocator),
        };
    }
    pub fn deinit(self: *Runtime, allocator: Allocator) void {
        self.variableStack.deinit(allocator);
        self.argBuffer.deinit(allocator);
        self.callStack.deinit(allocator);
    }

    pub fn run(self: *Runtime, program: bytecode.Program) void {
        // Null handle
        _ = self.variableStack.push(Operand{ .item = 0 });
        // Return handle
        _ = self.variableStack.push(Operand{ .item = 0 });
        const code = program.instructions;
        var pointer = program.entryPoint;
        // If the main function doesn't return, we'll move all the way to the end and gracefully exit
        // tbh, we shouldn't allow this, but this is just to make sure we don't overflow
        while (pointer < code.len) {
            const ins = code[pointer];
            switch (ins.op.op) {
                .pushArgument => {
                    const val = switch (ins.op.argType) {
                        .literalAHandleB, .bothLiteral => ins.a,
                        .handleALiteralB, .bothHandle => self.variableStack.backing[@truncate(ins.a.item)],
                    };
                    _ = self.argBuffer.push(val);
                },
                .call => {
                    self.callStack.push(Call{ .varStackHeight = self.variableStack.height(), .returnPosition = pointer + 1 });
                    const args = self.argBuffer.backing[0..self.argBuffer.used];
                    for (args) |a| {
                        _ = self.variableStack.push(a);
                    }
                    pointer = @intCast(ins.a.item);
                },
                .ret => {
                    // callStack.pop() is only null if there is no stack left. In this case, we are done.
                    const previousCall = self.callStack.pop() orelse return;
                    const height = self.variableStack.height();
                    if (previousCall.varStackHeight > height) {
                        @panic("Calling convention was violated!");
                    }
                    for (0..height - previousCall.varStackHeight) |_| {
                        _ = self.variableStack.pop();
                    }
                    pointer = previousCall.returnPosition;
                },
                .pushItem => {
                    const val = switch (ins.op.argType) {
                        .literalAHandleB, .bothLiteral => ins.a,
                        .handleALiteralB, .bothHandle => self.variableStack.backing[@truncate(ins.a.item)],
                    };
                    _ = self.variableStack.push(val);
                },
                .pop => _ = self.variableStack.pop(),
                else => {
                    const a: u64 = switch (ins.op.argType) {
                        .literalAHandleB, .bothLiteral => ins.a.item,
                        .handleALiteralB, .bothHandle => self.variableStack.backing[@truncate(ins.a.item)].item,
                    };
                    const b: u64 = switch (ins.op.argType) {
                        .handleALiteralB, .bothLiteral => ins.b.item,
                        .literalAHandleB, .bothHandle => self.variableStack.backing[@truncate(ins.b.item)].item,
                    };
                    const result: u64 = switch (ins.op.op) {
                        .move => a,
                        .negateBool => @intCast(@intFromBool(!(a != 0))),
                        .negateNumber => @bitCast(-@as(f64, @bitCast(a))),
                        .noop => 0,
                        .add => @bitCast(@as(f64, @bitCast(a)) + @as(f64, @bitCast(b))),
                        .subtract => @bitCast(@as(f64, @bitCast(a)) - @as(f64, @bitCast(b))),
                        .multiply => @bitCast(@as(f64, @bitCast(a)) * @as(f64, @bitCast(b))),
                        .divide => @bitCast(@as(f64, @bitCast(a)) / @as(f64, @bitCast(b))),
                        .modulo => @bitCast(@mod(@as(f64, @bitCast(a)), @as(f64, @bitCast(b)))),
                        .neq => @as(u64, @intCast(@intFromBool(!std.math.approxEqAbs(f64, @bitCast(a), @bitCast(b), 5 * std.math.floatEps(f64))))),
                        .eq => @as(u64, @intCast(@intFromBool(!std.math.approxEqAbs(f64, @bitCast(a), @bitCast(b), 5 * std.math.floatEps(f64))))),
                        .ge => @as(u64, @intCast(@intFromBool(@as(f64, @bitCast(a)) >= @as(f64, @bitCast(b))))),
                        .le => @as(u64, @intCast(@intFromBool(@as(f64, @bitCast(a)) <= @as(f64, @bitCast(b))))),
                        .greater => @as(u64, @intCast(@intFromBool(@as(f64, @bitCast(a)) > @as(f64, @bitCast(b))))),
                        .less => @as(u64, @intCast(@intFromBool(@as(f64, @bitCast(a)) < @as(f64, @bitCast(b))))),
                        .bAnd => @as(u64, @intCast(@intFromBool((a != 0) and (b != 0)))),
                        .bOr => @as(u64, @intCast(@intFromBool((a != 0) or (b != 0)))),
                        else => 0,
                    };
                    self.variableStack.backing[ins.dest] = Operand{ .item = result };
                },
            }
            pointer += 1;
        }
    }
};
