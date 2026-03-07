//! This is just for the actual Lox interpreter program. The actual interpreter is based in root.zig.
const std = @import("std");
const Io = std.Io;

const builtin = @import("builtin");

/// This imports the separate module containing `root.zig`. Take a look in `build.zig` for details.
const lib = @import("libzlox");
const scanning = lib.scanning;
const parsing = lib.parsing;
const runtime = lib.runtime;
const bytecode = lib.bytecode;
const errors = lib.errors;
const context = lib.context;
const ErrorLog = errors.ErrorLog;
const ErrorTrace = errors.ErrorTrace;

const Context = context.Context;

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

    var stderrBuf: [1520]u8 = undefined;
    var stderrWriter = std.fs.File.stderr().writer(&stderrBuf);
    const stderr = &stderrWriter.interface;
    defer stderr.flush() catch @panic("write to stderr failed!");

    const operation = functionMap.get(args.next() orelse "") orelse .unknown;

    const path = args.next() orelse {
        _ = try stderr.write("No file provided!");
        return;
    };

    const u32Max = std.math.maxInt(u32);

    const contents = reading: {
        const cwd = std.fs.cwd();
        var file = cwd.openFile(path, .{ .mode = .read_only }) catch {
            const cwdDir = try cwd.realpathAlloc(gpa, ".");
            defer gpa.free(cwdDir);
            try stderr.print("File {s} did not exist\nCWD is listed as {s}\n", .{ path, cwdDir });
            return;
        };
        defer file.close();
        var readerBuffer: [1024]u8 = undefined;
        var freader = file.reader(&readerBuffer);
        const reader = &freader.interface;

        break :reading reader.allocRemaining(gpa, .limited(u32Max)) catch |e| switch (e) {
            error.ReadFailed, error.OutOfMemory => return e,
            error.StreamTooLong => {
                try stderr.print("file is too large: maximum permissible file size is {d} bytes", .{u32Max});
                return;
            },
        };
    };
    defer gpa.free(contents);

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const astAlloc = arena.allocator();

    var iter = scanning.TokenIterator.init(contents);
    var errLog = try ErrorLog.init(astAlloc);
    const ctx = Context{ .tokenIterator = &iter, .log = &errLog };

    switch (operation) {
        .tokenize => {
            var tokens = try std.ArrayList(scanning.Token).initCapacity(gpa, contents.len);
            defer tokens.deinit(gpa);

            while (true) {
                const token = iter.next(&errLog).token;
                if (token.tokenType == .eof) {
                    _ = try stderr.write("EOF  null\n");
                    break;
                }

                if (token.tokenType == .invalidChar) {
                    try stderr.print("[line {d}] Error: Unexpected character: {s}\n", .{ iter.lineNumber, iter.exchangeTokenForSource(token) });
                } else {
                    try scanning.printToken(&iter, token, stderr);
                }
            }
        },
        .parse => {
            // an expression can never be less than 1 token
            var codegen = try bytecode.BytecodeGenerator.init(astAlloc);

            parsing.parseAndCompileAll(ctx, &codegen);

            const errsOrNull = errLog.recover();
            if (errsOrNull) |errs| {
                try stderr.print("found {d} compilation errors:\n", .{errs.len});
                for (errs) |trace| {
                    try handleErrorTrace(trace, ctx, stderr);
                }
                return;
            }

            for (codegen.bytecodeList.items) |ins| {
                try bytecode.printInstruction(ins, stderr);
            }
        },
        .evaluate => {
            var codegen = try bytecode.BytecodeGenerator.init(astAlloc);

            _ = try stderr.write("\nbytecode:\n");

            parsing.parseAndCompileAll(ctx, &codegen);

            const errsOrNull = errLog.recover();
            if (errsOrNull) |errs| {
                try stderr.print("found {d} compilation errors:\n", .{errs.len});
                for (errs) |trace| {
                    try handleErrorTrace(trace, ctx, stderr);
                }
                return;
            }

            const programOrNull = codegen.finalize(ctx);
            if (programOrNull) |program| {
                try stderr.print("( ENTRY POINT {d} )\n", .{program.entryPoint});
                for (program.instructions) |ins| {
                    try bytecode.printInstruction(ins, stderr);
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

fn handleErrorTrace(trace: ErrorTrace, ctx: Context, writer: *Io.Writer) !void {
    const iter = ctx.tokenIterator;

    const line: []u8, const hlOffset, const hlLen = a: {
        switch (trace.where) {
            .token => |t| {
                const whereSrc = iter.exchangeTokenForSource(t.t);
                const tok = t.t;
                const line = iter.exchangeTokenForLine(tok);
                const lineInt = @intFromPtr(line.ptr);
                const tokPosInt = @intFromPtr(whereSrc.ptr);
                break :a .{ line, tokPosInt - lineInt, whereSrc.len };
            },
            .sourceRange => |s| {
                const end = @min(s.endPossiblyOOB, @as(u32, @intCast(iter.source.len)));
                const line = iter.getSourceLinesInRange(s.start, end);
                const hlOffset = @as(u32, @truncate(@intFromPtr(line.ptr))) - s.start;
                const hlLen = end - s.start;
                break :a .{ line, hlOffset, hlLen };
            },
            .eof => {
                const line = iter.getLineWithEOF();
                // shhh this prevents underflow
                break :a if (line.len > 0) .{ line, line.len - 1, 1 } else .{ @constCast(" "), 0, 1 };
            },
        }
    };
    const lineBeforeHl, const lineHl, const lineAfterHl = .{ line[0..hlOffset], line[hlOffset .. hlOffset + hlLen], line[hlOffset + hlLen ..] };

    try writer.print("error:\n{d}: {s}\x1b[31;1m{s}\x1b[0m{s}\n", .{ trace.lineNumber, lineBeforeHl, lineHl, lineAfterHl });
    try writer.print("{d}: ", .{trace.lineNumber});
    _ = try writer.write("\x1b[31;1m");
    for (0..hlOffset) |_| {
        try writer.writeByte('-');
    }
    for (0..hlLen) |_| {
        try writer.writeByte('^');
    }
    _ = try writer.write("\x1b[0m\n");

    try trace.printSelf(writer);
    _ = try writer.write("\n\n");
    try writer.flush();
}
