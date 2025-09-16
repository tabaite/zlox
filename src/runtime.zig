// Any type value that is not the given built-in values is
// a user-defined class.

const std = @import("std");
const parsing = @import("parsing.zig");
const Allocator = std.mem.Allocator;

pub const RuntimeError = error{
    StackOverflow,
};

pub const RuntimeIndex = u24;

pub const Type = enum(u8) {
    number,
    string,
    bool,
    nil,
    pub fn idx(self: @This()) u8 {
        return @intFromEnum(self);
    }
};

pub const OperatorType = enum(u32) {
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

    negate,
    negateBool,

    // temporary
    print,
};

fn Handle(repr: type, tag: []const u8) type {
    return enum(repr) {
        _,
        pub fn idx(self: @This()) u32 {
            return @intFromEnum(self);
        }
        // credit to @squirl from https://zig.news/msw/a-distinct-index-types-journey-12fp
        comptime {
            _ = tag;
        }
    };
}

pub const Operation = struct {
    op: OperatorType,
    operandA: VarHandle,
    operandB: VarHandle,
};

// A handle to a variable stored in the IRHandler.
pub const VarHandle = packed struct {
    handle: Handle(u24, "handle for the variable"),
    type: Type,
};

pub const VarInternal = struct {
    type: Type,

    // No, I do not want to use a union :P
    // Strings store the memory address of the slice.
    extra: u64,
};

pub const VarStack = struct {
    const STACKSIZE: usize = 16777215;

    items: []VarInternal,
    stringAllocator: Allocator,
    used: u24,

    pub fn init(allocator: Allocator, stringAllocator: Allocator) !VarStack {
        return .{
            .items = try allocator.alloc(VarInternal, STACKSIZE),
            .used = 0,
            .stringAllocator = stringAllocator,
        };
    }

    fn push(self: *VarStack, t: Type, extra: u64) !VarHandle {
        if (self.used >= STACKSIZE) {
            return RuntimeError.StackOverflow;
        }

        const handle = self.used;
        self.items[handle] = .{ .extra = extra, .type = t };
        self.used += 1;

        return .{ .handle = @enumFromInt(handle), .type = t };
    }

    pub fn pushNumber(self: *VarStack, number: f64) !VarHandle {
        return self.push(.number, @bitCast(number));
    }

    pub fn pushBool(self: *VarStack, b: bool) !VarHandle {
        return self.push(.bool, @intCast(@intFromBool(b)));
    }

    pub fn pushString(self: *VarStack, str: []u8) !VarHandle {
        const newString = try self.stringAllocator.dupe(u8, str);
        return self.push(.string, @intCast(@intFromPtr(&newString)));
    }

    pub fn peek(self: *VarStack) ?VarHandle {
        if (self.used > 0) {
            const top = self.items[self.used];
            return .{ .handle = self.used, .type = top.type };
        }
        return null;
    }

    pub fn pop(self: *VarStack) void {
        if (self.used > 0) {
            self.used -= 1;
        }
    }
};

pub const Runtime = struct {
    variableStack: VarStack,
    pub fn init(allocator: Allocator, stringAllocator: Allocator) !Runtime {
        return .{ .variableStack = try VarStack.init(allocator, stringAllocator) };
    }
};
