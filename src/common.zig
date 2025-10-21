const std = @import("std");
const Allocator = std.mem.Allocator;

fn Stack(T: type, BackingSize: comptime_int) type {
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
