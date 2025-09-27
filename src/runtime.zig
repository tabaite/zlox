// Any type value that is not the given built-in values is
// a user-defined class.

const std = @import("std");
const bytecode = @import("bytecode.zig");
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

pub const VarStack = struct {
    const NILHANDLE: Handle = 0;
    const STACKSIZE: usize = 16777215;

    items: []Operand,
    allocator: Allocator,
    stringAllocator: Allocator,
    used: u32,

    pub fn init(allocator: Allocator, stringAllocator: Allocator) !VarStack {
        const items = try allocator.alloc(Operand, STACKSIZE);
        items[0] = .nil;
        return .{
            .items = items,
            .used = 1,
            .allocator = allocator,
            .stringAllocator = stringAllocator,
        };
    }

    pub fn deinit(self: *VarStack) void {
        for (0..self.used - 1) |_| {
            self.pop();
        }
        self.allocator.free(self.items);
    }

    fn push(self: *VarStack, v: Operand) !Handle {
        if (self.used >= STACKSIZE) {
            return RuntimeError.StackOverflow;
        }

        const handle = self.used;
        self.items[handle] = v;
        self.used += 1;

        return .{ .handle = handle, .type = v };
    }

    pub fn set(self: *VarStack, handle: Handle, new: Operand) void {
        self.items[handle.handle] = new;
    }

    pub fn get(self: *VarStack, handle: Handle) Operand {
        return self.items[handle.handle];
    }

    pub fn pop(self: *VarStack) void {
        // The first element is our nil handle.
        if (self.used > 1) {
            self.used -= 1;
        }
    }
};

pub const Runtime = struct {
    variableStack: VarStack,
    pub fn init(allocator: Allocator, stringAllocator: Allocator) !Runtime {
        return .{
            .varRegistry = std.StringHashMap(Handle).init(allocator),
            .variableStack = try VarStack.init(allocator, stringAllocator),
        };
    }
    pub fn deinit(self: *Runtime) void {
        self.variableStack.deinit();
        self.varRegistry.deinit();
    }

    pub fn run(self: *Runtime, code: []bytecode.Instruction) void {
        for (code) |ins| {
            switch (ins.op) {
                .pushItem => self.variableStack.push(ins.a),
                else => {
                    const a: u64 = switch (ins.op.argType) {
                        .literalAHandleB, .bothLiteral => ins.a.item,
                        .handleALiteralB, .bothHandle => self.variableStack.get(@truncate(ins.a.item)),
                    };
                    const b: u64 = switch (ins.op.argType) {
                        .literalAHandleB, .bothLiteral => ins.b.item,
                        .handleALiteralB, .bothHandle => self.variableStack.get(@truncate(ins.b.item)),
                    };
                    const result: u64 = switch (ins.op.op) {
                        .move => a,
                        .negateBool => @intCast(@intFromBool(!(a != 0))),
                        .negateNumber => @bitCast(-@as(f64, @floatFromInt(a))),
                        .noop => 0,
                        .add => @bitCast(@as(f64, @floatFromInt(a)) + @as(f64, @floatFromInt(b))),
                        .subtract => @bitCast(@as(f64, @floatFromInt(a)) - @as(f64, @floatFromInt(b))),
                        .multiply => @bitCast(@as(f64, @floatFromInt(a)) * @as(f64, @floatFromInt(b))),
                        .divide => @bitCast(@as(f64, @floatFromInt(a)) / @as(f64, @floatFromInt(b))),
                        .modulo => @bitCast(@as(f64, @floatFromInt(a)) % @as(f64, @floatFromInt(b))),
                        .neq => @bitCast(@intFromBool(!std.math.approxEqAbs(f64, @bitCast(a), @bitCast(b), 5 * std.math.floatEps(f64)))),
                        .eq => @bitCast(@intFromBool(!std.math.approxEqAbs(f64, @bitCast(a), @bitCast(b), 5 * std.math.floatEps(f64)))),
                        .ge => @bitCast(@intFromBool(@as(f64, @floatFromInt(a)) >= @as(f64, @floatFromInt(b)))),
                        .le => @bitCast(@intFromBool(@as(f64, @floatFromInt(a)) <= @as(f64, @floatFromInt(b)))),
                        .greater => @bitCast(@intFromBool(@as(f64, @floatFromInt(a)) > @as(f64, @floatFromInt(b)))),
                        .less => @bitCast(@intFromBool(@as(f64, @floatFromInt(a)) < @as(f64, @floatFromInt(b)))),
                        .bAnd => @bitCast(@intFromBool((a != 0) and (b != 0))),
                        .bOr => @bitCast(@intFromBool((a != 0) or (b != 0))),
                        .pushItem => 0,
                    };
                    self.variableStack.set(ins.dest, .{ .item = result });
                },
            }
        }
    }
};
