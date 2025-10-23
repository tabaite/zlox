const std = @import("std");

// i love circular imports!
const parsing = @import("parsing.zig");
const bytecode = @import("bytecode.zig");

const Allocator = std.mem.Allocator;

pub const CompileError = parsing.ParsingError || bytecode.CompilationError;
pub const ErrorTrace = struct {
    err: CompileError,
    lineNum: u32,
    line: []u8,
};
pub const ErrorLog = struct {
    const BACKINGSIZE = 32767;
    backing: []ErrorTrace,
    used: usize,

    pub fn init(allocator: Allocator) !ErrorLog {
        return .{
            .backing = try allocator.alloc(ErrorTrace, BACKINGSIZE),
            .used = 0,
        };
    }

    pub fn push(self: *ErrorLog, trace: ErrorTrace) void {
        self.backing[self.used] = trace;
        self.used += 1;
    }

    pub fn recover(self: ErrorLog) ?[]ErrorTrace {
        if (self.used == 0) {
            return null;
        } else {
            return self.backing[0..self.used];
        }
    }
};

pub fn Stack(T: type, BackingSize: comptime_int) type {
    return struct {
        const Self = @This();
        backing: []T,
        used: usize,
        pub fn init(allocator: Allocator) !Self {
            return .{ .used = 0, .backing = try allocator.alloc(T, BackingSize) };
        }
        pub fn push(self: *Self, item: T) void {
            self.backing[self.used] = item;
            self.used += 1;
        }
        pub fn top(self: *Self) ?*T {
            if (self.used == 0) {
                return null;
            }
            return &self.backing[self.used - 1];
        }
        pub fn height(self: Self) usize {
            return self.used;
        }
        pub fn pop(self: *Self) ?T {
            if (self.used == 0) {
                return null;
            }
            const item = self.backing[self.used - 1];
            self.used -= 1;
            return item;
        }
        pub fn deinit(self: *Self, allocator: Allocator) void {
            allocator.free(self.backing);
        }
    };
}
