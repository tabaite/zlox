// Any type value that is not the given built-in values is
// a user-defined class.

const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Type = enum(u8) {
    number,
    string,
    bool,
    nil,
    // a little surprise for later
    // _,
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

// A handle to a variable stored in the IRHandler.
pub const VarHandle = packed struct {
    handle: Handle(u24, "handle for the variable"),
    type: Type,
};

pub const Operation = struct {
    op: OperatorType,
    operandA: VarHandle,
    operandB: VarHandle,
};

pub const IRCreator = struct {
    pub fn init(allocator: Allocator) IRCreator {
        return .{ .allocator = allocator };
    }
};
