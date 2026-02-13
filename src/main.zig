//! This is just for the actual Lox interpreter program. The actual interpreter is based in root.zig.
const std = @import("std");
const builtin = @import("builtin");

/// This imports the separate module containing `root.zig`. Take a look in `build.zig` for details.
const lib = @import("libzlox");
const scanning = lib.scanning;
const parsing = lib.parsing;
const runtime = lib.runtime;
const bytecode = lib.bytecode;
const errors = lib.errors;
const ErrorLog = errors.ErrorLog;
const ErrorTrace = errors.ErrorTrace;

pub const ProgramFunction = enum {
    unknown,
    tokenize,
    parse,
    evaluate,
};

pub const functionMap = std.StaticStringMap(ProgramFunction).initComptime(.{
    .{ "tokenize", .tokenize },
    .{ "parse", .parse },
    .{ "evaluate", .evaluate },
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

    const path = args.next() orelse {
        _ = try stderr.write("No file provided!");
        return;
    };

    const contents = reading: {
        const cwd = std.fs.cwd();
        const file = cwd.openFile(path, .{}) catch {
            const cwdDir = try cwd.realpathAlloc(gpa, ".");
            defer gpa.free(cwdDir);
            try stderr.print("File {s} did not exist\nCWD is listed as {s}\n", .{ path, cwdDir });
            return;
        };
        defer file.close();

        const reader = file.reader();
        break :reading try reader.readAllAlloc(gpa, 2_000_000_000);
    };

    const stderrAny = stderr.any();
    const u32Max = std.math.maxInt(u32);
    if (contents.len > u32Max) {
        try stderrAny.print("file is too large: maximum permissible file size is {d} bytes, file is {d} bytes", .{ u32Max, contents.len });
    }

    defer gpa.free(contents);

    var iter = scanning.TokenIterator.init(contents);

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const astAlloc = arena.allocator();

    var errLog = try ErrorLog.init(astAlloc, &iter);

    switch (operation) {
        .tokenize => {
            var tokens = try std.ArrayList(scanning.Token).initCapacity(gpa, contents.len);
            defer tokens.deinit();

            while (iter.next(&errLog)) |token| {
                if (token.tokenType == .invalidChar) {
                    try stderr.print("[line {d}] Error: Unexpected character: {s}\n", .{ iter.lineNumber, iter.exchangeTokenForSource(token) });
                } else {
                    try tokens.append(token);
                }
            }

            for (tokens.items) |t| {
                try scanning.printToken(&iter, t, stderr.any());
            }
            _ = try stderr.write("EOF  null\n");
        },
        .parse => {
            // an expression can never be less than 1 token
            var codegen = try bytecode.BytecodeGenerator.init(astAlloc);
            var astParser = parsing.AstParser.new(&iter, &errLog);

            try astParser.parseAndCompileAll(&codegen, &errLog);

            const errs = errLog.recover();
            if (errs != null) {
                for (errs.?) |trace| {
                    try handleErrorTrace(trace, &iter, stderrAny);
                }
                return;
            }

            for (codegen.bytecodeList.items) |ins| {
                try bytecode.printInstruction(ins, stderrAny);
            }
        },
        .evaluate => {
            var codegen = try bytecode.BytecodeGenerator.init(astAlloc);
            var astParser = parsing.AstParser.new(&iter, &errLog);

            _ = try stderr.write("\nbytecode:\n");

            try astParser.parseAndCompileAll(&codegen, &errLog);

            const errs = errLog.recover();
            if (errs != null) {
                for (errs.?) |trace| {
                    try handleErrorTrace(trace, &iter, stderrAny);
                }
                return;
            }

            const programOrNull = codegen.finalize(&errLog);
            if (programOrNull) |program| {
                try stderr.print("( ENTRY POINT {d} )\n", .{program.entryPoint});
                for (program.instructions) |ins| {
                    try bytecode.printInstruction(ins, stderrAny);
                }

                _ = try stderr.write("\nevaluating\n");
                var rt = try runtime.Runtime.init(astAlloc, gpa);
                defer rt.deinit(astAlloc);
                rt.run(program);

                if (rt.variableStack.used > 2) {
                    try stderr.print("expected all items cleaned up, found {d} extra items\n", .{rt.variableStack.used});
                }
            }
        },
        .unknown => {
            try stderr.print("Usage: ./your_program ( tokenize | parse | evaluate ) <filename>\n", .{});
        },
    }
}

fn handleErrorTrace(trace: ErrorTrace, context: *scanning.TokenIterator, out: std.io.AnyWriter) !void {
    try out.print("error:\n{d}: \x1b[31;1m{s}\x1b[0m\n", .{ trace.lineNum, trace.line });
    switch (trace.err) {
        .illegalToken => |t| try out.print("illegal token: \"{s}\" is not recognized as a valid token", .{t.token}),
        .expectedToken => |e| {
            if (e.found) |found| {
                try out.print("expected {s}, found {s} ( \"{s}\" )", .{ e.expected.typeAsString(), found.tokenType.typeAsString(), context.exchangeTokenForSource(found) });
            } else {
                try out.print("expected {s}, found the end of the file", .{e.expected.typeAsString()});
            }
        },
        else => _ = try out.write("man idk"),
    }
    try out.writeByteNTimes('\n', 2);
}
