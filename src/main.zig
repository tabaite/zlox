//! This is just for the actual Lox interpreter program. The actual interpreter is based in root.zig.
const std = @import("std");
const builtin = @import("builtin");

/// This imports the separate module containing `root.zig`. Take a look in `build.zig` for details.
const lib = @import("libzlox");
const scanning = lib.scanning;
const parsing = lib.parsing;
const evaluation = lib.evaluation;
const runtime = lib.runtime;

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

    const stderrAny = stderr.any();

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

            var codegen = try lib.bytecode.BytecodeGenerator.init(astAlloc);
            var astParser = parsing.AstParser.new(&iter);
            while (astParser.nextStatement(&codegen, astAlloc) catch |e| err: {
                try handleParseError(e, stderrAny, astParser.iter.source, astParser.iter.position);
                break :err null;
            }) |s| {
                try parsing.printStatement(s, stderrAny);
            }
        },
        .evaluate => {
            var arena = std.heap.ArenaAllocator.init(gpa);
            defer arena.deinit();
            const astAlloc = arena.allocator();

            var codegen = try lib.bytecode.BytecodeGenerator.init(astAlloc);
            var astParser = parsing.AstParser.new(&iter);
            while (astParser.nextStatement(&codegen, astAlloc) catch |e| err: {
                try handleParseError(e, stderrAny, astParser.iter.source, astParser.iter.position);
                break :err null;
            }) |s| {
                try statementList.append(s);
            }

            _ = try stderr.write("\nevaluating:\n");

            const program = astParser.programTree(astAlloc) catch |e| {
                try handleParseError(e, stderrAny, astParser.iter.source, astParser.iter.position, astParser.lastToken orelse .{ .tokenType = .invalidChar, .source = null });
                return;
            };
            try parsing.printBlock(program, stderrAny);

            var evaluator = evaluation.Evaluator.init(try runtime.Runtime.init(astAlloc, gpa));
            defer evaluator.deinit();

            _ = try evaluator.evaluateBlock(astAlloc, program);
        },
        .unknown => {
            try stderr.print("Usage: ./your_program ( tokenize | parse | evaluate ) <filename>\n", .{});
        },
    }
}

fn handleParseError(err: anyerror, out: std.io.AnyWriter, source: []u8, position: usize, offendingToken: scanning.Token) !void {
    switch (err) {
        parsing.ParsingError.ExpectedSemicolon => _ = try out.write("expected semicolon\n"),
        parsing.ParsingError.ExpectedClosingBrace => _ = try out.write("expected closing brace\n"),
        parsing.ParsingError.ExpectedToken => _ = try out.write("expected a token\n"),
        parsing.ParsingError.ExpectedIdentifier => _ = try out.write("expected a name"),
        parsing.ParsingError.UnexpectedToken => _ = try out.write("unexpected token!\n"),
        else => return err,
    }
    _ = try out.write("token:\n");
    try scanning.printToken(offendingToken, out);
    _ = try out.write("\n");
    _ = try out.write(source[0..position]);
    _ = try out.write("<HERE>");
    _ = try out.write(source[position..]);
}

fn handleRuntimeError(err: anyerror, out: std.io.AnyWriter) !void {
    const RuntimeError = runtime.RuntimeError;
    const EvaluationError = evaluation.EvaluationError;
    switch (err) {
        EvaluationError.IncompatibleTypesForOperands => _ = try out.write("types are incompatible!\n"),
        EvaluationError.NoOperationForOperands => _ = try out.write("could not find a suitable operation for operands!\n"),
        RuntimeError.OutOfStackBounds => _ = try out.write("oob\n"),
        RuntimeError.StackOverflow => _ = try out.write("stack OVERFLOW!"),
        RuntimeError.UndeclaredVariableAccessed => _ = try out.write("tried to access a variable that does not exist within the current scope"),
        RuntimeError.VariableAlreadyDeclared => _ = try out.write("this variable name has already been declared!"),
        else => return err,
    }
}
