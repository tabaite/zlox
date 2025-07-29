//! This is just for the actual Lox interpreter program. The actual interpreter is based in root.zig.
const std = @import("std");
const builtin = @import("builtin");

/// This imports the separate module containing `root.zig`. Take a look in `build.zig` for details.
const lib = @import("libzlox");
const scanning = lib.scanning;

pub const ProgramFunction = enum {
    unknown,
    tokenize,
    parse,
};

pub const functionMap = std.StaticStringMap(ProgramFunction).initComptime(.{
    .{ "tokenize", .tokenize },
    .{ "parse", .parse },
});

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

    const operation = functionMap.get(args.next() orelse "") orelse .unknown;

    switch (operation) {
        .parse => {
            _ = try stderr.write("parsing coming soon to a zlox near you\n");
        },
        .tokenize => {
            const path = args.next() orelse {
                _ = try stderr.write("No file provided!");
                return;
            };

            const contents = reading: {
                const cwd = std.fs.cwd();
                const file = try cwd.openFile(path, .{});
                defer file.close();

                const reader = file.reader();
                break :reading try reader.readAllAlloc(gpa, 2_000_000_000);
            };
            defer gpa.free(contents);

            var iter = scanning.TokenIterator.init(contents);

            var error_char: u8 = undefined;

            while (iter.next(&error_char) catch |err| syn: {
                switch (err) {
                    scanning.SyntaxError.UnexpectedCharacter => {
                        _ = try stderr.print("[line {d}] Error: Unexpected character: {c}\n", .{ iter.line_number, error_char });
                    },
                    scanning.SyntaxError.UnterminatedString => {
                        _ = try stderr.write("unterminated string (FIX THIS ERROR MESSAGE)\n");
                    },
                }
                break :syn scanning.Token{ .token_type = .invalid, .source = undefined };
            }) |token| {
                try printToken(token, stderr.any());
            }

            _ = try stderr.write("EOF  null\n");
        },
        .unknown => {
            try stderr.print("Usage: ./your_program tokenize <filename>\n", .{});
        },
    }
}

fn printToken(token: scanning.Token, out: std.io.AnyWriter) !void {
    _ = switch (token.token_type) {
        .bang => try out.write("BANG ! null\n"),
        .bang_equal => try out.write("BANG_EQUAL != null\n"),
        .less => try out.write("LESS < null\n"),
        .less_equal => try out.write("LESS_EQUAL <= null\n"),
        .greater => try out.write("GREATER > null\n"),
        .greater_equal => try out.write("GREATER >= null\n"),
        .equal => try out.write("EQUAL = null\n"),
        .equal_equal => try out.write("EQUAL_EQUAL == null\n"),
        .left_paren => try out.write("LEFT_PAREN ( null\n"),
        .right_paren => try out.write("RIGHT_PAREN ) null\n"),
        .left_brace => try out.write("LEFT_BRACE { null\n"),
        .right_brace => try out.write("RIGHT_BRACE } null\n"),
        .comma => try out.write("COMMA , null\n"),
        .dot => try out.write("DOT . null\n"),
        .minus => try out.write("MINUS - null\n"),
        .plus => try out.write("PLUS + null\n"),
        .semicolon => try out.write("SEMICOLON ; null\n"),
        .star => try out.write("STAR * null\n"),
        .slash => try out.write("SLASH / null\n"),

        .kw_and => try out.write("AND and null\n"),
        .kw_class => try out.write("CLASS class null\n"),
        .kw_else => try out.write("ELSE else null\n"),
        .kw_false => try out.write("FALSE false null\n"),
        .kw_fun => try out.write("FUN fun null\n"),
        .kw_for => try out.write("FOR for null\n"),
        .kw_if => try out.write("IF if null\n"),
        .kw_nil => try out.write("NIL nil null\n"),
        .kw_or => try out.write("OR or null\n"),
        .kw_print => try out.write("PRINT print null\n"),
        .kw_return => try out.write("RETURN return null\n"),
        .kw_super => try out.write("SUPER super null\n"),
        .kw_this => try out.write("THIS this null\n"),
        .kw_true => try out.write("TRUE true null\n"),
        .kw_var => try out.write("VAR var null\n"),
        .kw_while => try out.write("WHILE while null\n"),

        .number => {
            const str = token.source orelse "";
            try out.print("NUMBER {s} <NUMBER>\n", .{str});
        },
        .string => {
            const str = token.source orelse "";
            try out.print("STRING \"{s}\" {s}\n", .{ str, str });
        },
        .identifier => {
            const str = token.source orelse "";
            try out.print("IDENTIFIER {s} null\n", .{str});
        },
        else => unreachable,
    };
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
