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

pub const CallStack = common.Stack(usize, 10000);

pub const VarStack = common.Stack(Operand, 16777215);

pub const Runtime = struct {
    stringAllocator: Allocator,
    variableStack: VarStack,
    callStack: CallStack,
    pub fn init(allocator: Allocator, stringAllocator: Allocator) !Runtime {
        return .{
            .stringAllocator = stringAllocator,
            .variableStack = try VarStack.init(allocator),
            .callStack = try CallStack.init(allocator),
        };
    }
    pub fn deinit(self: *Runtime, allocator: Allocator) void {
        self.variableStack.deinit(allocator);
        self.callStack.deinit(allocator);
    }

    pub fn run(self: *Runtime, program: bytecode.Program) void {
        _ = self.variableStack.push(Operand{ .item = 0 });
        _ = self.callStack.push(0);
        const code = program.instructions;
        var pointer = program.entryPoint;
        // If the main function doesn't return, we'll move all the way to the end and gracefully exit
        // tbh, we shouldn't allow this, but this is just to make sure we don't overflow
        while (self.callStack.used > 0 and pointer < code.len) {
            const ins = code[pointer];
            switch (ins.op.op) {
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
