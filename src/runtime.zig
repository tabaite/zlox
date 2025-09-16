// Any type value that is not the given built-in values is
// a user-defined class.

const std = @import("std");
const parsing = @import("parsing.zig");
const Allocator = std.mem.Allocator;

pub const RuntimeError = error{
    StackOverflow,
    OutOfStackBounds,
    UndeclaredVariableAccessed,
    VariableAlreadyDeclared,
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

// A handle to a variable stored in the IRHandler.
pub const VarHandle = packed struct {
    handle: u24,
    type: Type,
};

pub const VarInternal = struct {
    type: Type,

    // No, I do not want to use a union :P
    // Strings store the memory address of the slice.
    extra: u64,
};

pub const VarStack = struct {
    const NILHANDLE: VarHandle = .{ .type = .nil, .handle = 0 };
    const STACKSIZE: usize = 16777215;

    items: []VarInternal,
    stringAllocator: Allocator,
    used: u24,

    pub fn init(allocator: Allocator, stringAllocator: Allocator) !VarStack {
        const items = try allocator.alloc(VarInternal, STACKSIZE);
        items[0] = .{ .type = .nil, .extra = 0 };
        return .{
            .items = items,
            .used = 1,
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

    pub fn set(self: *VarStack, handle: VarHandle, new: parsing.Literal) !void {
        if (handle.handle > self.used) {
            return RuntimeError.OutOfStackBounds;
        }

        const item = &self.items[handle.handle];
        if (item.type == .string) {
            self.stringAllocator.free(@ptrFromInt(@as(usize, item.extra)));
        }

        switch (new) {
            .number => |n| {
                item.* = VarInternal{ .type = .number, .extra = @bitCast(n) };
            },
            .bool => |b| {
                item.* = VarInternal{ .type = .bool, .extra = @intCast(@intFromBool(b)) };
            },
            .nil => {
                item.* = VarInternal{ .type = .nil, .extra = 0 };
            },
            .string => |s| {
                const strPtr = try self.stringAllocator.create([]u8);
                strPtr.* = s;
                item.* = VarInternal{ .type = .string, .extra = @bitCast(strPtr) };
            },
        }
    }

    // We might not need to dupe the string. I'm still undecided on when the
    // lifetime of the input file ends.
    pub fn pushString(self: *VarStack, str: []u8) !VarHandle {
        const newString = try self.stringAllocator.dupe(u8, str);
        // this is fine... i think
        const strPtr = try self.stringAllocator.create([]u8);
        strPtr.* = newString;
        return self.push(.string, @bitCast(strPtr));
    }

    pub fn pop(self: *VarStack) void {
        const handle = self.items[self.used];
        if (handle.type == .string) {
            self.stringAllocator.free(@ptrFromInt(@as(usize, handle.extra)));
        }

        // The first element is our nil handle.
        if (self.used > 1) {
            self.used -= 1;
        }
    }
};

pub const Runtime = struct {
    varRegistryAllocator: Allocator,
    varRegistry: std.StringHashMap(VarHandle),
    variableStack: VarStack,
    pub fn init(allocator: Allocator, stringAllocator: Allocator) !Runtime {
        return .{
            .varRegistryAllocator = allocator,
            .varRegistry = std.StringHashMap(VarHandle).init(allocator),
            .variableStack = try VarStack.init(allocator, stringAllocator),
        };
    }

    pub fn declare(self: *Runtime, name: []u8, v: ?parsing.Literal) !VarHandle {
        const handle = if (v != null) try self.push(v orelse unreachable) else VarHandle.NILHANDLE;
        const result = try self.varRegistry.getOrPut(name);
        if (result.found_existing) {
            return RuntimeError.VariableAlreadyDeclared;
        } else {
            result.value_ptr.* = handle;
        }
    }

    pub fn set(self: *Runtime, name: []u8, new: parsing.Literal) !void {
        const result = try self.varRegistry.getOrPut(name);
        if (result.found_existing) {
            const handle = result.value_ptr.*;
            self.variableStack.set(handle, new);
        } else {
            return RuntimeError.UndeclaredVariableAccessed;
        }
    }

    fn push(self: *Runtime, v: parsing.Literal) !VarHandle {
        switch (v) {
            .number => |n| return self.variableStack.pushNumber(n),
            .string => |s| return self.variableStack.pushString(s),
            .bool => |b| return self.variableStack.pushBool(b),
            // bro...... trust me
            .nil => unreachable,
        }
    }
};
