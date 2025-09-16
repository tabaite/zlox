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

const Type = parsing.Type;
const Variable = parsing.Literal;

// A handle to a variable stored in the IRHandler.
pub const VarHandle = packed struct {
    handle: u24,
    type: Type,
};

pub const VarStack = struct {
    const NILHANDLE: VarHandle = .{ .type = .nil, .handle = 0 };
    const STACKSIZE: usize = 16777215;

    items: []Variable,
    stringAllocator: Allocator,
    used: u24,

    pub fn init(allocator: Allocator, stringAllocator: Allocator) !VarStack {
        const items = try allocator.alloc(Variable, STACKSIZE);
        items[0] = .nil;
        return .{
            .items = items,
            .used = 1,
            .stringAllocator = stringAllocator,
        };
    }

    pub fn deinit(self: *VarStack) void {
        for (0..self.used - 1) |_| {
            self.pop();
        }
    }

    fn push(self: *VarStack, v: Variable) !VarHandle {
        if (self.used >= STACKSIZE) {
            return RuntimeError.StackOverflow;
        }

        const handle = self.used;
        self.items[handle] = val: {
            if (v == .string) {
                const str = try self.stringAllocator.dupe(u8, v.string);
                const newv = Variable{ .string = str };
                break :val newv;
            } else {
                break :val v;
            }
        };
        self.used += 1;

        return .{ .handle = handle, .type = v };
    }

    pub fn set(self: *VarStack, handle: VarHandle, new: Variable) !void {
        if (handle.handle > self.used) {
            return RuntimeError.OutOfStackBounds;
        }

        const item = &self.items[handle.handle];
        if (item.* == .string) {
            self.stringAllocator.free(item.string);
        }

        switch (new) {
            .number => |n| {
                item.* = .{ .number = n };
            },
            .bool => |b| {
                item.* = .{ .bool = b };
            },
            .nil => {
                item.* = .nil;
            },
            .string => |s| {
                const str = try self.stringAllocator.dupe(u8, s);
                item.* = Variable{ .string = str };
            },
        }
    }

    pub fn get(self: *VarStack, handle: VarHandle) !Variable {
        if (handle.handle > self.used) {
            return RuntimeError.OutOfStackBounds;
        }
        return self.items[handle.handle];
    }

    pub fn pop(self: *VarStack) void {
        const handle = self.items[self.used];
        if (handle == .string) {
            self.stringAllocator.free(handle.string);
        }

        // The first element is our nil handle.
        if (self.used > 1) {
            self.used -= 1;
        }
    }
};

pub const Runtime = struct {
    varRegistry: std.StringHashMap(VarHandle),
    variableStack: VarStack,
    pub fn init(allocator: Allocator, stringAllocator: Allocator) !Runtime {
        return .{
            .varRegistry = std.StringHashMap(VarHandle).init(allocator),
            .variableStack = try VarStack.init(allocator, stringAllocator),
        };
    }
    pub fn deinit(self: *Runtime) void {
        self.varRegistry.deinit();
        self.variableStack.deinit();
    }

    pub fn declare(self: *Runtime, name: []u8, v: ?Variable) !VarHandle {
        std.debug.print("Declared {s}\n", .{name});
        const handle = if (v != null) try self.push(v orelse unreachable) else VarStack.NILHANDLE;
        const result = try self.varRegistry.getOrPut(name);
        if (result.found_existing) {
            std.debug.print("huh???\n", .{});
            return RuntimeError.VariableAlreadyDeclared;
        } else {
            result.value_ptr.* = handle;
        }
        std.debug.print("Set {s}\n", .{name});
        return handle;
    }

    pub fn get(self: *Runtime, handle: VarHandle) !Variable {
        return try self.variableStack.get(handle);
    }

    pub fn set(self: *Runtime, name: []u8, new: Variable) !void {
        const result = try self.varRegistry.getOrPut(name);
        if (result.found_existing) {
            const handle = result.value_ptr.*;
            self.variableStack.set(handle, new);
        } else {
            return RuntimeError.UndeclaredVariableAccessed;
        }
    }

    fn push(self: *Runtime, v: Variable) !VarHandle {
        return self.variableStack.push(v);
    }
};
