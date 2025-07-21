//! This is just for the actual Lox interpreter program. The actual interpreter is based in root.zig.
const std = @import("std");
const builtin = @import("builtin");

/// This imports the separate module containing `root.zig`. Take a look in `build.zig` for details.
const lib = @import("libzlox");

pub fn main() !void {
    var debug = std.heap.DebugAllocator(.{}){};
    defer _ = debug.deinit();
    const gpa = switch (builtin.mode) {
        .Debug => debug.allocator(),
        .ReleaseFast, .ReleaseSafe, .ReleaseSmall => std.heap.c_allocator,
    };

    var args = try std.process.argsWithAllocator(gpa);
    defer args.deinit();
    // first arg will be our program
    _ = args.next();

    const stderr_file = std.io.getStdErr().writer();
    var bw = std.io.bufferedWriter(stderr_file);
    defer bw.flush() catch unreachable;

    const stderr = bw.writer();

    const operation = args.next() orelse return;

    if (std.mem.eql(u8, operation, "tokenize")) {
        const file = args.next() orelse {
            _ = try stderr.write("No file provided!");
            return;
        };
        try stderr.print("Tokenizing file {s}\n", .{file});
    } else {
        try stderr.print("Usage: ./your_program tokenize <filename>\n", .{});
    }

    try stderr.print("Run `zig build test` to run the tests.\n", .{});
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit();
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}

test "use other module" {
    try std.testing.expectEqual(@as(i32, 150), lib.add(100, 50));
}

test "fuzz example" {
    const Context = struct {
        fn testOne(context: @This(), input: []const u8) anyerror!void {
            _ = context;
            // Try passing `--fuzz` to `zig build test` and see if it manages to fail this test case!
            try std.testing.expect(!std.mem.eql(u8, "canyoufindme", input));
        }
    };
    try std.testing.fuzz(Context{}, Context.testOne, .{});
}
