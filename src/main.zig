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
                try printToken(t, stderr.any());
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

            var astParser = parsing.AstParser.new(&iter);
            const astRoot = try astParser.parse(astAlloc);

            _ = try stderr.write("\nevaluating:\n");

            const stderrAny = stderr.any();
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

                try printResult(result, stderrAny);
                _ = try stderrAny.write("\n");
                current = actual.next;
            }
        },
        .unknown => {
            try stderr.print("Usage: ./your_program tokenize <filename>\n", .{});
        },
    }
}

fn printToken(token: scanning.Token, out: std.io.AnyWriter) !void {
    _ = switch (token.tokenType) {
        .bang => try out.write("BANG ! null\n"),
        .bangEqual => try out.write("BANG_EQUAL != null\n"),
        .less => try out.write("LESS < null\n"),
        .lessEqual => try out.write("LESS_EQUAL <= null\n"),
        .greater => try out.write("GREATER > null\n"),
        .greaterEqual => try out.write("GREATER >= null\n"),
        .equal => try out.write("EQUAL = null\n"),
        .equalEqual => try out.write("EQUAL_EQUAL == null\n"),
        .leftParen => try out.write("LEFT_PAREN ( null\n"),
        .rightParen => try out.write("RIGHT_PAREN ) null\n"),
        .leftBrace => try out.write("LEFT_BRACE { null\n"),
        .rightBrace => try out.write("RIGHT_BRACE } null\n"),
        .comma => try out.write("COMMA , null\n"),
        .dot => try out.write("DOT . null\n"),
        .minus => try out.write("MINUS - null\n"),
        .plus => try out.write("PLUS + null\n"),
        .semicolon => try out.write("SEMICOLON ; null\n"),
        .star => try out.write("STAR * null\n"),
        .slash => try out.write("SLASH / null\n"),

        .kwAnd => try out.write("AND and null\n"),
        .kwClass => try out.write("CLASS class null\n"),
        .kwElse => try out.write("ELSE else null\n"),
        .kwFalse => try out.write("FALSE false null\n"),
        .kwFun => try out.write("FUN fun null\n"),
        .kwFor => try out.write("FOR for null\n"),
        .kwIf => try out.write("IF if null\n"),
        .kwNil => try out.write("NIL nil null\n"),
        .kwOr => try out.write("OR or null\n"),
        .kwPrint => try out.write("PRINT print null\n"),
        .kwReturn => try out.write("RETURN return null\n"),
        .kwSuper => try out.write("SUPER super null\n"),
        .kwThis => try out.write("THIS this null\n"),
        .kwTrue => try out.write("TRUE true null\n"),
        .kwVar => try out.write("VAR var null\n"),
        .kwWhile => try out.write("WHILE while null\n"),

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

fn printResult(result: evaluation.Result, out: std.io.AnyWriter) !void {
    switch (result) {
        .bool => |b| try out.print("{s}", .{if (b) "true" else "false"}),
        .number => |n| try out.print("{d}", .{n}),
        .string => |str| try out.print("{s}", .{str}),
        .nil => _ = try out.write("nil"),
    }
}
