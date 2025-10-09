//! This is just for the actual Lox interpreter program. The actual interpreter is based in root.zig.
const std = @import("std");
const builtin = @import("builtin");

/// This imports the separate module containing `root.zig`. Take a look in `build.zig` for details.
const lib = @import("libzlox");
const scanning = lib.scanning;
const parsing = lib.parsing;
const runtime = lib.runtime;
const bytecode = lib.bytecode;

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

            var codegen = try bytecode.BytecodeGenerator.init(astAlloc);
            var astParser = parsing.AstParser.new(&iter);

            astParser.parseAndCompileAll(&codegen, astAlloc) catch |e| {
                try handleParseError(e, stderrAny, iter, astParser.lastToken orelse .{ .tokenType = .invalidChar, .source = null });
                return;
            };

            for (codegen.bytecodeList.items) |ins| {
                try bytecode.printInstruction(ins, stderrAny);
            }
        },
        .evaluate => {
            var arena = std.heap.ArenaAllocator.init(gpa);
            defer arena.deinit();
            const astAlloc = arena.allocator();

            var codegen = try bytecode.BytecodeGenerator.init(astAlloc);
            var astParser = parsing.AstParser.new(&iter);

            _ = try stderr.write("\nbytecode:\n");

            astParser.parseAndCompileAll(&codegen, astAlloc) catch |e| {
                try handleParseError(e, stderrAny, iter, astParser.lastToken orelse .{ .tokenType = .invalidChar, .source = null });
                return;
            };

            for (codegen.bytecodeList.items) |ins| {
                try bytecode.printInstruction(ins, stderrAny);
            }

            _ = try stderr.write("\nevaluating\n");
            var rt = try runtime.Runtime.init(astAlloc, gpa);
            defer rt.deinit();
            rt.run(codegen.bytecodeList.items);

            _ = try stderr.write("\nreally hacky stack vis:\n");
            _ = try stderr.write("( NULL )\n");
            for (1..rt.variableStack.used) |i| {
                try bytecode.printInstruction(.{
                    .a = rt.variableStack.items[i],
                    .b = .NULL_HANDLE,
                    .dest = @truncate(i),
                    .op = .{ .argType = .bothLiteral, .op = .pushItem },
                }, stderrAny);
            }
        },
        .unknown => {
            try stderr.print("Usage: ./your_program ( tokenize | parse | evaluate ) <filename>\n", .{});
        },
    }
}

fn handleParseError(err: anyerror, out: std.io.AnyWriter, source: scanning.TokenIterator, offendingToken: scanning.Token) !void {
    const Parser = parsing.ParsingError;
    const CodeGen = bytecode.CompilationError;
    switch (err) {
        Parser.ExpectedSemicolon => _ = try out.write("expected semicolon\n"),
        Parser.ExpectedOpeningBrace => _ = try out.write("expected opening brace\n"),
        Parser.ExpectedClosingBrace => _ = try out.write("expected closing brace\n"),
        Parser.ExpectedOpeningParen => _ = try out.write("expected opening parenthesis\n"),
        Parser.ExpectedClosingParen => _ = try out.write("expected closing parenthesis\n"),
        Parser.ArgLimit128 => _ = try out.write("you can't have more arguments sorry\nhave you tried like a di framework or something\n"),
        Parser.ArgumentCannotBeTypeVoid => _ = try out.write("argument cannot have type \"void\"\n"),
        Parser.ExpectedToken => _ = try out.write("expected a token\n"),
        Parser.ExpectedIdentifier => _ = try out.write("expected a name\n"),
        Parser.UnexpectedToken => _ = try out.write("unexpected token!\n"),

        CodeGen.VariableNotDeclared => _ = try out.write("this variable doesn't exist in this scope!\n"),
        CodeGen.MainFunctionCannotHaveArgs => _ = try out.write("main function cannot have arguments\n"),
        CodeGen.MainFunctionCannotReturnValue => _ = try out.write("main function cannot return anything\n"),
        else => return err,
    }
    _ = try out.write("token:\n");
    try scanning.printToken(offendingToken, out);
    _ = try out.write("\n");

    const position = source.position;
    const line = source.lineNumber;
    const length = source.source.len;

    var lineStart: usize = 0;
    var lineEnd: usize = length;
    for (0..position) |t| {
        const pos = position - t;
        if (source.source[pos] == '\n') {
            lineStart = pos + 1;
            break;
        }
    }
    for (position..length) |pos| {
        if (source.source[pos] == '\n') {
            lineEnd = pos - 1;
            break;
        }
    }
    for (lineStart..lineEnd) |a| {
        switch (source.source[a]) {
            ' ', '\t', '\r' => {},
            else => {
                lineStart = a;
                break;
            },
        }
    }
    try out.print("line {d}: \x1b[31;1m{s}\x1b[0m\n", .{ line, source.source[lineStart..lineEnd] });
}
