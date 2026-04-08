//! This is just for the actual Lox interpreter program. The actual interpreter is based in root.zig.
const std = @import("std");
const ztracy = @import("ztracy");
const Io = std.Io;

const builtin = @import("builtin");

/// This imports the separate module containing `root.zig`. Take a look in `build.zig` for details.
const lib = @import("libzlox");
const scanning = lib.scanning;
const parsing = lib.parsing;
const ast = lib.ast;
const errors = lib.errors;
const context = lib.context;
const ErrorLog = errors.ErrorLog;
const ErrorTrace = errors.ErrorTrace;

const Context = context.Context;

pub const ProgramStage = enum(u32) {
    none = 0,
    tokenize = 1,
    parse = 2,
    evaluate = 3,

    pub fn asInt(self: ProgramStage) u32 {
        return @intFromEnum(self);
    }
};

pub const ProgramPipeline = struct {
    maxStage: ProgramStage,
    printTokens: bool = false,
    printInstructions: bool = false,
};

pub const pipelineMap = std.StaticStringMap(ProgramPipeline).initComptime(.{
    .{ "tokenize", ProgramPipeline{ .maxStage = .tokenize, .printTokens = true } },
    .{ "parse", ProgramPipeline{ .maxStage = .parse, .printInstructions = true } },
    .{ "evaluate", ProgramPipeline{ .maxStage = .evaluate } },
});

pub fn main() !void {
    ztracy.FrameMarkStart("runtime");
    defer ztracy.FrameMarkEnd("runtime");

    const mainZone = ztracy.ZoneN(@src(), "main function");
    defer mainZone.End();

    var debug = std.heap.DebugAllocator(.{}){};
    defer _ = debug.deinit();
    var tracy = ztracy.TracyAllocator.init(switch (builtin.mode) {
        .Debug => debug.allocator(),
        .ReleaseFast, .ReleaseSafe, .ReleaseSmall => std.heap.c_allocator,
    });
    const gpa = tracy.allocator();

    var args = try std.process.argsWithAllocator(gpa);
    defer args.deinit();
    // first arg will be our program
    _ = args.next();

    var stderrBuf: [4096]u8 = undefined;
    var stderrWriter = std.fs.File.stderr().writer(&stderrBuf);
    const stderr = &stderrWriter.interface;
    defer stderr.flush() catch @panic("write to stderr failed!");

    const pipeline = pipelineMap.get(args.next() orelse "") orelse {
        try stderr.print("Usage: ./your_program ( tokenize | parse | evaluate ) <filename>\n", .{});
        return;
    };

    const path = args.next() orelse {
        _ = try stderr.write("No file provided!");
        return;
    };

    const u32Max = std.math.maxInt(u32);

    const contents = reading: {
        const fopenZone = ztracy.ZoneN(@src(), "read source file contents");
        defer fopenZone.End();

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

    if (pipeline.maxStage.asInt() < ProgramStage.tokenize.asInt()) {
        return;
    }

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const astAlloc = arena.allocator();

    var iter = scanning.TokenIterator.init(contents);
    var errLog = try ErrorLog.init(astAlloc);
    const ctx = Context{ .tokenIterator = &iter, .log = &errLog };

    defer errLog.deinit(astAlloc);

    if (try tryPrintErrors(ctx, stderr)) {
        return;
    }
    if (pipeline.printTokens) {
        const printTokensZone = ztracy.ZoneN(@src(), "print token list");
        defer printTokensZone.End();

        var cloneIter = scanning.TokenIterator.init(contents);
        while (true) {
            const token = cloneIter.next(&errLog).token;
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
    }
    if (pipeline.maxStage.asInt() < ProgramStage.parse.asInt()) {
        return;
    }

    var astgen = ast.AST.init(astAlloc);
    defer astgen.deinit();

    parsing.parseAndCompileAll(ctx, &astgen);

    if (try tryPrintErrors(ctx, stderr)) {
        return;
    }
    if (pipeline.printInstructions) {
        _ = try stderr.write("bytecode printing currently not supported due to ast refactoring\n");
    }
    if (pipeline.maxStage.asInt() < ProgramStage.evaluate.asInt()) {
        return;
    }

    _ = try stderr.write("program execution currently not supported due to ast refactoring\n");
}

fn tryPrintErrors(ctx: Context, stderr: *Io.Writer) !bool {
    const log = ctx.log;
    const errsOrNull = log.recover();
    if (errsOrNull) |errs| {
        const tryPrintErrorZone = ztracy.ZoneN(@src(), "print compilation errors");
        defer tryPrintErrorZone.End();

        try stderr.print("found {d} compilation errors:\n", .{errs.len});
        for (errs) |trace| {
            try handleErrorTrace(trace, ctx, stderr);
        }
        return true;
    } else {
        return false;
    }
}

fn handleErrorTrace(trace: ErrorTrace, ctx: Context, writer: *Io.Writer) !void {
    const handleTraceZone = ztracy.ZoneN(@src(), "print error trace");
    defer handleTraceZone.End();

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

    // 4_294_967_295
    var intParseBuffer: [11]u8 = undefined;
    const intParsedLen = std.fmt.printInt(&intParseBuffer, trace.lineNumber, 10, .upper, .{});

    try writer.print("error:\n{s}: {s}\x1b[31;1m{s}\x1b[0m{s}\n", .{ intParseBuffer[0..intParsedLen], lineBeforeHl, lineHl, lineAfterHl });
    _ = try writer.write("\x1b[31;1m");
    // the two extra characters are the colon + space
    for (0..hlOffset + intParsedLen + 2) |_| {
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
