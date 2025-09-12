//! This is just for the actual Lox interpreter program. The actual interpreter is based in root.zig.
const std = @import("std");
const builtin = @import("builtin");

/// This imports the separate module containing `root.zig`. Take a look in `build.zig` for details.
const lib = @import("libzlox");
const scanning = lib.scanning;
const parsing = lib.parsing;
const evaluation = lib.evaluation;

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
        const file = try cwd.openFile(path, .{});
        defer file.close();

        const reader = file.reader();
        break :reading try reader.readAllAlloc(gpa, 2_000_000_000);
    };
    defer gpa.free(contents);

    var iter = scanning.TokenIterator.init(contents);

    switch (operation) {
        .tokenize => {
            var tokens = try std.ArrayList(scanning.Token).initCapacity(gpa, contents.len);
            defer tokens.deinit();

            while (iter.next()) |token| {
                if (token.tokenType == .invalidChar) {
                    try stderr.print("[line {d}] Error: Unexpected character: {s}\n", .{ iter.lineNumber, token.source orelse "NULL???" });
                } else if (token.tokenType == .unterminatedString) {
                    _ = try stderr.write("unterminated string (FIX THIS ERROR MESSAGE)\n");
                } else {
                    try tokens.append(token);
                }
            }

            for (tokens.items) |t| {
                try scanning.printToken(t, stderr.any());
            }
            _ = try stderr.write("EOF  null\n");
        },
        .parse => {
            // an expression can never be less than 1 token
            var arena = std.heap.ArenaAllocator.init(gpa);
            defer arena.deinit();
            const astAlloc = arena.allocator();

            var astParser = parsing.AstParser.new(&iter);
            const astRoot = try astParser.parse(astAlloc);
            try parsing.printStatements(astRoot, stderr.any());
        },
        .evaluate => {
            var arena = std.heap.ArenaAllocator.init(gpa);
            defer arena.deinit();
            const astAlloc = arena.allocator();

            const stderrAny = stderr.any();

            var astParser = parsing.AstParser.new(&iter);
            const astRoot = astParser.parse(astAlloc) catch |e| {
                const pos = astParser.iter.position;
                switch (e) {
                    parsing.ParsingError.ExpectedSemicolon => _ = try stderr.write("expected semicolon\n"),
                    parsing.ParsingError.ExpectedClosingBrace => _ = try stderr.write("expected closing brace\n"),
                    parsing.ParsingError.ExpectedToken => _ = try stderr.write("expected a token\n"),
                    parsing.ParsingError.UnexpectedToken => _ = try stderr.write("unexpected token!\n"),
                    error.OutOfMemory => _ = try stderr.write("out of memory :(\n"),
                    else => return e,
                }
                _ = try stderr.write("token:\n");
                if (astParser.tryPeek() != null) {
                    try scanning.printToken(astParser.tryPeek() orelse unreachable, stderrAny);
                }
                _ = try stderr.write("\n");
                _ = try stderr.write(astParser.iter.source[0..pos]);
                _ = try stderr.write("<HERE>");
                _ = try stderr.write(astParser.iter.source[pos..]);
                return;
            };

            _ = try stderr.write("\nevaluating:\n");

            var current: ?*parsing.Statement = astRoot;
            while (current != null) {
                const actual = current orelse unreachable;
                try parsing.printExpression(actual.expr, stderrAny);
                _ = try stderrAny.write(" - ");
                const result = evaluation.evaluateNode(astAlloc, actual.expr) catch |err| e: {
                    const EvaluationError = evaluation.EvaluationError;
                    if (err == EvaluationError.IncompatibleTypesForOperands) {
                        _ = try stderr.write("types are incompatible!\n");
                    } else if (err == EvaluationError.NoOperationForOperands) {
                        _ = try stderr.write("could not find a suitable operation for operands!\n");
                    }

                    break :e .nil;
                };

                try evaluation.printResult(result, stderrAny);
                _ = try stderrAny.write("\n");
                current = actual.next;
            }
        },
        .unknown => {
            try stderr.print("Usage: ./your_program tokenize <filename>\n", .{});
        },
    }
}
