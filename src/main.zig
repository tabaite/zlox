//! This is just for the actual Lox interpreter program. The actual interpreter is based in root.zig.
const std = @import("std");
const builtin = @import("builtin");

/// This imports the separate module containing `root.zig`. Take a look in `build.zig` for details.
const lib = @import("libzlox");
const scanning = lib.scanning;
const parsing = lib.parsing;
const runtime = lib.runtime;
const bytecode = lib.bytecode;
const ErrorLog = lib.common.ErrorLog;

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
            var astParser = parsing.AstParser.new(&iter, astAlloc);

            var errLog = try ErrorLog.init(astAlloc);
            try astParser.parseAndCompileAll(&codegen, &errLog);

            const errs = errLog.recover();
            if (errs != null) {
                for (errs.?) |trace| {
                    try handleParseError(trace, stderrAny);
                }
                return;
            }

            for (codegen.bytecodeList.items) |ins| {
                try bytecode.printInstruction(ins, stderrAny);
            }
        },
        .evaluate => {
            var arena = std.heap.ArenaAllocator.init(gpa);
            defer arena.deinit();
            const astAlloc = arena.allocator();

            var codegen = try bytecode.BytecodeGenerator.init(astAlloc);
            var astParser = parsing.AstParser.new(&iter, astAlloc);

            _ = try stderr.write("\nbytecode:\n");

            var errLog = try ErrorLog.init(astAlloc);
            try astParser.parseAndCompileAll(&codegen, &errLog);

            const errs = errLog.recover();
            if (errs != null) {
                for (errs.?) |trace| {
                    try handleParseError(trace, stderrAny);
                }
                return;
            }

            const program = try codegen.finalize();

            try stderr.print("( ENTRY POINT {d} )\n", .{program.entryPoint});
            for (program.instructions) |ins| {
                try bytecode.printInstruction(ins, stderrAny);
            }

            _ = try stderr.write("\nevaluating\n");
            var rt = try runtime.Runtime.init(astAlloc, gpa);
            defer rt.deinit(astAlloc);
            rt.run(program);

            _ = try stderr.write("\nreally hacky stack vis:\n");
            _ = try stderr.write("( NULL )\n");
            for (1..rt.variableStack.used) |i| {
                try bytecode.printInstruction(.{
                    .a = rt.variableStack.backing[i],
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

fn handleParseError(trace: parsing.ErrorTrace, out: std.io.AnyWriter) !void {
    const Error = lib.common.CompileError;
    const message = switch (trace.err) {
        Error.ExpectedSemicolon => "expected semicolon\n",
        Error.ExpectedOpeningBrace => "expected opening brace\n",
        Error.ExpectedClosingBrace => "expected closing brace\n",
        Error.ExpectedOpeningParen => "expected opening parenthesis\n",
        Error.ExpectedClosingParen => "expected closing parenthesis\n",
        Error.ArgLimit128 => "you can't have more arguments sorry\nhave you tried like a di framework or something\n",
        Error.ArgumentCannotBeTypeVoid => "argument cannot have type \"void\"\n",
        Error.ExpectedToken => "expected a token\n",
        Error.ExpectedKwFun => "expected the keyword \"fun\"\n",
        Error.ExpectedComma => "expected a comma\n",
        Error.ExpectedIdentifier => "expected a name\n",
        Error.UnexpectedToken => "unexpected token!\n",
        Error.ExpectedTypeAtVariableDeclaration => "expected a type at this variable declaration (either inferred from initial value or explicitly provided)",
        Error.GlobalScopeNoLongerUsable => "you can't put statements in global scope anymore :) (put it in main() pls) (global variable declarations will be supported soon i promise)",
        Error.VariableNotDeclared => "this variable doesn't exist in this scope!\n",
        Error.VariableAlreadyDeclared => "a variable with the same name has already been declared in this scope\n",
        Error.MainFunctionCannotHaveArgs => "main function cannot have arguments\n",
        Error.MainFunctionCannotReturnValue => "main function cannot return anything\n",
        else => return trace.err,
    };

    try out.print("error: {s}\nline {d}: \x1b[31;1m{s}\x1b[0m\n\n", .{ message, trace.lineNum, trace.line });
}
