const scanning = @import("scanning.zig");
const std = @import("std");
const bytecode = @import("bytecode.zig");
const ast = @import("ast.zig");
const context = @import("context.zig");
const ztracy = @import("ztracy");

const NULL_HANDLE = ast.NULL_HANDLE;
const MAX_ARGS = ast.MAX_ARGS;

const Token = scanning.Token;
const TokenContext = scanning.TokenContext;
const AST = ast.AST;
const Allocator = std.mem.Allocator;
const AnyWriter = std.io.AnyWriter;
const Context = context.Context;

const StmtRange = ast.Range;
const ExprHandle = ast.ExprHandle;

pub const VERYBADPRINTFUNCTIONNAME = "printtttt!!";

pub const BinaryExprType = enum {
    equality,
    notEquality,
    bOr,
    bAnd,
    greater,
    greaterEqual,
    less,
    lessEqual,
    add,
    subtract,
    multiply,
    divide,
    modulo,

    pub fn asVerb(self: BinaryExprType) []const u8 {
        return switch (self) {
            .equality => "equality",
            .notEquality => "in-equality",
            .bOr => "binary-or",
            .bAnd => "binary-and",
            .greater => "greater-than",
            .greaterEqual => "greater-than-or-equal",
            .less => "less-than",
            .lessEqual => "less-equal",
            .add => "addition",
            .subtract => "subtraction",
            .multiply => "multiplication",
            .divide => "division",
            .modulo => "modulo",
        };
    }
};

pub const UnaryExprType = enum {
    negate,
    negateBool,

    pub fn asVerb(self: UnaryExprType) []const u8 {
        return switch (self) {
            .negate => "negation",
            .negateBool => "binary-not",
        };
    }
};

const TokenToBinaryExpr = struct {
    key: scanning.TokenType,
    value: BinaryExprType,
};

const BlockReturnInfo = struct {
    // Not used yet (apart from implicit returns)
    // But will be useful for control flow in the future.
    // We don't need to concern ourselves with the type. The return
    // rule will submit the return type to the bytecode generator,
    // and generate a CompilationError if it is wrong. I really need
    // to stop shoving compilation errors into the parser.
    returnsOnAllPaths: bool,
};

const ParseInterruptSignal = error{
    /// Any semicolon encountered within the parse MUST be interpreted as an immediate end to
    /// the statement.
    /// This signal can also be used for the end of a file.
    ReachedParen,
    ReachedSemicolon,
    ReachedBrace,
    ReachedEOF,
};
/// Interrupt levels define the scope of interrupts.
/// Each level is a superset of the previous, so
/// a Parenthesis interrupt level can also invoke a
/// Semicolon interrupt, and so on, so forth.
const InterruptLevel = enum(u32) {
    /// For either arguments in a function call
    /// or in a expression group. Note that we
    /// "eagerly" parse parenthesis (that is, we match the first end to the latest start)
    ///
    /// Example of eager closing:
    /// ( ( ) <- would close the second pair, not the first
    ///
    /// Example of potential interrupt locations:
    ///   _____ - expression that will be given the "Parenthesis" interrupt level
    /// ( 15 * ) <- expected expression, found right paren
    /// ( 15 * ; <- expected expression, found semicolon
    /// ( 15 * } <- expected expression, found right brace
    /// ( 15 * EOF <- expected expression, found EOF
    parenthesis = 3,

    /// For most statements.
    ///
    /// Example of potential interrupt locations:
    /// ____________ - expression that will be given the "Semicolon" interrupt level
    /// var result = ; <- expected expression, found semicolon
    /// var result = } <- expected expression, found right brace
    /// var result = EOF <- expected expression, found EOF
    semicolon = 2,

    /// For anything within a block that doesn't count as a statement.
    brace = 1,

    /// For anything that can only be interrupted by the end of the file.
    ///
    /// Example of potential interrupt locations:
    /// _____________ - expression that will be given the "EOF" interrupt level
    /// fun foo ( EOF <- expected right paren, found EOF
    eof = 0,

    pub fn asInt(self: InterruptLevel) u32 {
        return @intFromEnum(self);
    }
};

fn isInterruptAtExact(sig: ParseInterruptSignal, target: InterruptLevel) bool {
    const PIS = ParseInterruptSignal;
    const sigLevel: InterruptLevel = switch (sig) {
        PIS.ReachedParen => .parenthesis,
        PIS.ReachedSemicolon => .semicolon,
        PIS.ReachedBrace => .brace,
        PIS.ReachedEOF => .eof,
    };
    return target == sigLevel;
}

fn isInterruptAtOrAbove(sig: ParseInterruptSignal, target: InterruptLevel) bool {
    const IntError = std.meta.Int(.unsigned, @bitSizeOf(anyerror));
    const PIS = ParseInterruptSignal;

    // kinda hacky haha
    const largerThanSet: @Vector(4, IntError) = switch (target) {
        .parenthesis => .{
            @intFromError(PIS.ReachedParen),
            @intFromError(PIS.ReachedSemicolon),
            @intFromError(PIS.ReachedBrace),
            @intFromError(PIS.ReachedEOF),
        },
        .semicolon => .{
            @intFromError(PIS.ReachedSemicolon),
            @intFromError(PIS.ReachedSemicolon),
            @intFromError(PIS.ReachedBrace),
            @intFromError(PIS.ReachedEOF),
        },
        .brace => .{
            @intFromError(PIS.ReachedBrace),
            @intFromError(PIS.ReachedBrace),
            @intFromError(PIS.ReachedBrace),
            @intFromError(PIS.ReachedEOF),
        },
        .eof => .{
            @intFromError(PIS.ReachedEOF),
            @intFromError(PIS.ReachedEOF),
            @intFromError(PIS.ReachedEOF),
            @intFromError(PIS.ReachedEOF),
        },
    };
    const mask: @Vector(4, IntError) = @splat(@intFromError(sig));
    const matches: @Vector(4, bool) = mask == largerThanSet;
    return @reduce(.Or, matches);
}

// HELPERS
inline fn matchTokenToExprOrNull(target: scanning.TokenType, comptime matches: []const TokenToBinaryExpr) ?BinaryExprType {
    const tracyZone = ztracy.ZoneN(@src(), "match token to binary expression");
    defer tracyZone.End();

    inline for (matches) |t| {
        if (t.key == target) {
            return t.value;
        }
    }
    return null;
}

inline fn peekOrInterrupt(ctx: Context, level: InterruptLevel) ParseInterruptSignal!TokenContext {
    const simd = std.simd;

    const tracyZone = ztracy.ZoneN(@src(), "peek token stream or interrupt");
    defer tracyZone.End();

    const tokenContext = ctx.tokenIterator.peek(ctx.log);
    const token = tokenContext.token;
    const MatchVec = @Vector(4, u32);
    const TT = scanning.TokenType;
    const PIS = ParseInterruptSignal;

    const interruptMatches: [4]MatchVec = .{
        .{ @intFromEnum(TT.eof), 0, 0, 0 },
        .{ @intFromEnum(TT.eof), @intFromEnum(TT.rightBrace), 0, 0 },
        .{ @intFromEnum(TT.eof), @intFromEnum(TT.rightBrace), @intFromEnum(TT.semicolon), 0 },
        .{ @intFromEnum(TT.eof), @intFromEnum(TT.rightBrace), @intFromEnum(TT.semicolon), @intFromEnum(TT.rightParen) },
    };
    const interruptErrs: [4]PIS = .{
        PIS.ReachedEOF,
        PIS.ReachedBrace,
        PIS.ReachedSemicolon,
        PIS.ReachedParen,
    };

    const mask: MatchVec = @splat(@intFromEnum(token.tokenType));
    const interruptResult = interruptMatches[level.asInt()] == mask;
    return if (simd.firstTrue(interruptResult)) |idx| interruptErrs[idx] else tokenContext;
}

inline fn advance(ctx: Context) void {
    _ = ctx.tokenIterator.next(ctx.log);
}

const TokenFilterError = error{
    DoesNotMatch,
};

// Tries to "filter" a token through a match. If no match, return null, and log the error.
inline fn filterCurrentTokenOrErr(tt: scanning.TokenType, ctx: Context, interruptLevel: InterruptLevel) (TokenFilterError || ParseInterruptSignal)!Token {
    const tracyZone = ztracy.ZoneN(@src(), "try match current token");
    defer tracyZone.End();

    const iter = ctx.tokenIterator;
    const log = ctx.log;
    // minimum interrupt level is eof
    const tokenContext = peekOrInterrupt(ctx, interruptLevel) catch |e| {
        // shhhhhhh
        log.push(.{ .expectedToken = .{ .expected = tt } }, iter.peek(log));
        return e;
    };
    if (tokenContext.token.tokenType != tt) {
        log.push(.{ .expectedToken = .{ .expected = tt } }, tokenContext);
        return TokenFilterError.DoesNotMatch;
    }
    return tokenContext.token;
}

// The way this AST parser works is somewhat simple.
// Each rule, described by the table above is a function.
// The function mutates the state of the parser, moving the position forward
// to the token immediately after the expression it returns.
pub fn parseAndCompileAll(ctx: Context, astgen: *AST) void {
    const parseZone = ztracy.ZoneN(@src(), "parse + compile");
    defer parseZone.End();

    while (peekOrInterrupt(ctx, .eof)) |_| {
        functionDeclarationRule(ctx, astgen);
    } else |_| {}
}

fn functionDeclarationRule(ctx: Context, astgen: *AST) void {
    const tracyZone = ztracy.ZoneN(@src(), "parse function declaration");
    defer tracyZone.End();

    const iter = ctx.tokenIterator;
    _ = filterCurrentTokenOrErr(.kwFun, ctx, .eof) catch {};
    advance(ctx);

    const funNameTOrEof = filterCurrentTokenOrErr(.identifier, ctx, .eof);
    advance(ctx);

    _ = filterCurrentTokenOrErr(.leftParen, ctx, .eof) catch {};
    advance(ctx);

    var args: [MAX_ARGS][]u8 = undefined;
    var argCount: usize = 0;

    // if EOF, main ( EOF,
    // skip parsing arguments
    // should be fine to implement this hack
    const argStart = peekOrInterrupt(ctx, .eof) catch TokenContext{
        .newPos = 0,
        .lineNumber = 0,
        .token = .{
            .tokenType = .rightParen,
            .sourceEndExclusive = 0,
            .sourceStart = 0,
        },
    };
    if (argStart.token.tokenType != .rightParen) while (peekOrInterrupt(ctx, .eof)) |_| {
        const argName = filterCurrentTokenOrErr(.identifier, ctx, .eof) catch break;

        advance(ctx);
        const typeDesignatorOrInterrupt = peekOrInterrupt(ctx, .eof);
        if (typeDesignatorOrInterrupt) |typeDesignatorCtx| {
            const typeDesignator = typeDesignatorCtx.token;
            switch (typeDesignator.tokenType) {
                .comma => {
                    ctx.pushError(.expectedTypeAnnotation);
                },

                .colon => {
                    advance(ctx);
                    const argTypeOrInterrupt = peekOrInterrupt(ctx, .eof);
                    if (argTypeOrInterrupt) |_| {
                        args[argCount] = iter.exchangeTokenForSource(argName);
                    } else |_| {
                        ctx.pushError(.expectedTypeToken);
                    }
                    advance(ctx);
                },

                else => {
                    ctx.pushError(.expectedTypeAnnotation);
                    break;
                },
            }
        } else |_| {}

        const continuationOrInterrupt = peekOrInterrupt(ctx, .eof);
        if (continuationOrInterrupt) |continuation| {
            switch (continuation.token.tokenType) {
                .rightParen => {
                    if (argCount == MAX_ARGS - 1) {
                        // TODO: rework this so that we continue parsing, but not recording arguments after the limit is reached.
                        ctx.pushError(.argLimitExceeded);
                        break;
                    } else {
                        argCount += 1;
                    }
                    break;
                },
                .comma => {
                    advance(ctx);
                    if (argCount == MAX_ARGS - 1) {
                        // TODO: rework this so that we continue parsing, but not recording arguments after the limit is reached.
                        ctx.pushError(.argLimitExceeded);
                        break;
                    } else {
                        argCount += 1;
                    }
                },
                else => ctx.pushError(.{ .expectedToken = .{ .expected = .rightParen } }),
            }
        } else |_| {}
    } else |_| {};

    _ = filterCurrentTokenOrErr(.rightParen, ctx, .eof) catch {};
    advance(ctx);

    const returnTypeTokenOrNull = peekOrInterrupt(ctx, .eof) catch TokenContext{ .newPos = 0, .lineNumber = 0, .token = .{ .tokenType = .invalidChar, .sourceStart = 0, .sourceEndExclusive = 0 } };
    switch (returnTypeTokenOrNull.token.tokenType) {
        .tyBool, .tyNum, .tyString, .tyVoid => {
            advance(ctx);
        },
        // Start of function body. We assume this means void.
        //         v
        .leftBrace => {},
        else => {
            advance(ctx);
            ctx.pushError(.expectedTypeToken);
        },
    }

    const argNames = args[0..argCount];
    // Function body
    const stmts = blockRule(ctx, astgen);

    if (funNameTOrEof) |funNameT| {
        astgen.newFunction(ctx.tokenIterator.exchangeTokenForSource(funNameT), argNames, stmts);
    } else |_| {}
}

fn blockRule(ctx: Context, astgen: *AST) StmtRange {
    const tracyZone = ztracy.ZoneN(@src(), "parse block");
    defer tracyZone.End();

    const defaultRange: StmtRange = .EMPTY;
    _ = filterCurrentTokenOrErr(.leftBrace, ctx, .eof) catch return defaultRange;
    advance(ctx);

    const retInfo = blockBodyRule(ctx, astgen);

    _ = filterCurrentTokenOrErr(.rightBrace, ctx, .eof) catch return retInfo;
    advance(ctx);
    return retInfo;
}

fn blockBodyRule(ctx: Context, astgen: *AST) StmtRange {
    const tracyZone = ztracy.ZoneN(@src(), "parse block body");
    defer tracyZone.End();

    const stmtStart: u32 = @truncate(astgen.statementList.items.len);

    while (peekOrInterrupt(ctx, .brace)) |t| {
        const tk = t.token;
        switch (tk.tokenType) {
            .kwIf => ifRule(ctx, astgen) catch {},
            .leftBrace => _ = blockRule(ctx, astgen),
            else => statementRule(ctx, astgen) catch {
                // interrupted by brace and returned early
                // do NOT advance otherwise the brace will be skipped
                //
                // advancing on eof will still result in eof so it's fine
                break;
            },
        }
    } else |_| {}
    // when we are interrupted by either EOF or right brace
    const stmtEnd: u32 = @truncate(astgen.statementList.items.len);
    _ = filterCurrentTokenOrErr(.rightBrace, ctx, .eof) catch {};
    return .{ .start = stmtStart, .end = stmtEnd };
}

fn ifRule(ctx: Context, astgen: *AST) !void {
    const tracyZone = ztracy.ZoneN(@src(), "parse if statement");
    defer tracyZone.End();

    _ = try filterCurrentTokenOrErr(.kwIf, ctx, .brace);
    advance(ctx);

    _ = filterCurrentTokenOrErr(.leftParen, ctx, .semicolon) catch |e| {
        switch (e) {
            TokenFilterError.DoesNotMatch => {},
            // if ( ;
            // -----^ skip over this...
            //
            // if ( }
            // -----^ don't skip over this...
            else => |interrupt| if (!isInterruptAtOrAbove(interrupt, .brace)) {
                advance(ctx);
                return interrupt;
            },
        }
    };
    advance(ctx);

    // use later
    _ = expressionRule(ctx, astgen, .semicolon) catch |e| {
        // if (expr ;
        // -----^ skip over this...
        //
        // if (expr }
        // -----^ don't skip over this...
        if (!isInterruptAtOrAbove(e, .brace)) {
            advance(ctx);
        }
    };

    _ = filterCurrentTokenOrErr(.rightParen, ctx, .semicolon) catch |e| {
        switch (e) {
            TokenFilterError.DoesNotMatch => {},
            else => |interrupt| if (!isInterruptAtOrAbove(interrupt, .brace)) {
                advance(ctx);
            },
        }
    };
    advance(ctx);

    const body = try peekOrInterrupt(ctx, .semicolon);
    switch (body.token.tokenType) {
        .leftBrace => _ = blockRule(ctx, astgen),
        else => try statementNoDeclRule(ctx, astgen),
    }
}

fn baseStatementRule(ctx: Context, astgen: *AST, firstRule: fn (Context, *AST) ParseInterruptSignal!void, dbgName: [*:0]const u8) !void {
    const tracyZone = ztracy.ZoneN(@src(), dbgName);
    defer tracyZone.End();

    firstRule(ctx, astgen) catch |e| {
        if (isInterruptAtExact(e, .semicolon)) {
            // expr ;
            //      ^ position
            advance(ctx);
            return;
        } else {
            // expr ( } | EOF )
            _ = filterCurrentTokenOrErr(.semicolon, ctx, .brace) catch {};
        }
    };

    const semicolonMatchOrErr = filterCurrentTokenOrErr(.semicolon, ctx, .brace);
    if (semicolonMatchOrErr) |_| {
        advance(ctx);
    } else |err| {
        switch (err) {
            TokenFilterError.DoesNotMatch => {},
            else => return err,
        }
    }
}
/// Returns an interrupt on a brace/EOF.
fn statementRule(ctx: Context, astgen: *AST) !void {
    try baseStatementRule(ctx, astgen, declarationRule, "parse statement");
}

/// Returns an interrupt on a brace/EOF. No declarations allowed.
fn statementNoDeclRule(ctx: Context, astgen: *AST) !void {
    try baseStatementRule(ctx, astgen, assignmentRule, "parse statement (no declaration)");
}

fn declarationRule(ctx: Context, astgen: *AST) ParseInterruptSignal!void {
    const tracyZone = ztracy.ZoneN(@src(), "try parse declaration or fallthrough");
    defer tracyZone.End();

    const iter = ctx.tokenIterator;
    const decl = try peekOrInterrupt(ctx, .semicolon);
    if (decl.token.tokenType != .kwVar) {
        _ = try assignmentRule(ctx, astgen);
        return;
    }
    advance(ctx);

    const nameToken = try peekOrInterrupt(ctx, .semicolon);
    if (nameToken.token.tokenType != .identifier) {
        ctx.pushError(.{ .expectedToken = .{ .expected = .identifier } });
    }

    advance(ctx);
    const typeHintOrEqualsCtx = peekOrInterrupt(ctx, .semicolon) catch {
        ctx.pushError(.expectedTypeAnnotation);
        return ParseInterruptSignal.ReachedSemicolon;
    };
    const typeHintOrEquals = typeHintOrEqualsCtx.token;
    switch (typeHintOrEquals.tokenType) {
        .colon => {
            advance(ctx);

            const typeToken = peekOrInterrupt(ctx, .semicolon) catch |e| {
                // todo: fix
                ctx.pushError(.expectedTypeToken);
                return e;
            };
            _ = switch (typeToken.token.tokenType) {
                .tyBool => .bool,
                .tyNum => .number,
                .tyString => .string,
                .tyVoid => .nil,
                else => e: {
                    ctx.pushError(.expectedTypeToken);
                    break :e .nil;
                },
            };

            advance(ctx);

            const nextCtx = try peekOrInterrupt(ctx, .semicolon);
            const next = nextCtx.token;
            const initialValue: ExprHandle = val: {
                switch (next.tokenType) {
                    .equal => {
                        advance(ctx);
                        break :val try expressionRule(ctx, astgen, .semicolon);
                    },
                    .semicolon => {
                        break :val astgen.newExpression(.{ .literal = .nil });
                    },
                    else => {
                        ctx.pushError(.{ .expectedToken = .{ .expected = .semicolon } });
                        break :val astgen.newExpression(.{ .literal = .nil });
                    },
                }
            };

            _ = astgen.newStatement(.{
                .declaration = .{
                    .name = iter.exchangeTokenForSource(nameToken.token),
                    .val = initialValue,
                },
            });
        },
        .equal => {
            advance(ctx);
            _ = astgen.newStatement(.{
                .declaration = .{
                    .name = iter.exchangeTokenForSource(nameToken.token),
                    .val = try expressionRule(ctx, astgen, .semicolon),
                },
            });
        },
        // Includes semicolon.
        else => {
            ctx.pushError(.expectedTypeAnnotation);
        },
    }
}

fn assignmentRule(ctx: Context, astgen: *AST) !void {
    const tracyZone = ztracy.ZoneN(@src(), "try parse assignment or fallthrough");
    defer tracyZone.End();

    const prevPosition = ctx.tokenIterator.*;

    const name = try peekOrInterrupt(ctx, .semicolon);
    if (name.token.tokenType != .identifier) {
        return try returnRule(ctx, astgen);
    }

    advance(ctx);

    const eq = try peekOrInterrupt(ctx, .semicolon);
    if (eq.token.tokenType != .equal) {
        ctx.tokenIterator.* = prevPosition;
        return try returnRule(ctx, astgen);
    }

    advance(ctx);

    const val = try expressionRule(ctx, astgen, .semicolon);
    _ = astgen.newStatement(.{
        .assignment = .{
            .name = ctx.tokenIterator.exchangeTokenForSource(name.token),
            .val = val,
        },
    });
}

fn returnRule(ctx: Context, astgen: *AST) !void {
    const tracyZone = ztracy.ZoneN(@src(), "try parse return or fallthrough");
    defer tracyZone.End();

    const ret = try peekOrInterrupt(ctx, .semicolon);
    if (ret.token.tokenType != .kwReturn) {
        try declarationRule(ctx, astgen);
        return;
    }
    advance(ctx);
    if (peekOrInterrupt(ctx, .semicolon)) |_| {
        _ = astgen.newStatement(.{ .funReturn = try expressionRule(ctx, astgen, .semicolon) });
    } else |_| {}
}

fn expressionRule(ctx: Context, astgen: *AST, interruptLevel: InterruptLevel) ParseInterruptSignal!ExprHandle {
    const tracyZone = ztracy.ZoneN(@src(), "try parse expression");
    defer tracyZone.End();

    _ = peekOrInterrupt(ctx, interruptLevel) catch {
        ctx.pushError(.expectedExpression);
        return NULL_HANDLE;
    };
    return try orRule(ctx, astgen, interruptLevel);
}

// might be the most atrocious function body i've ever written
inline fn binaryRule(ctx: Context, astgen: *AST, comptime ruleName: [:0]const u8, comptime matches: []const TokenToBinaryExpr, previousRule: fn (Context, *AST, InterruptLevel) ParseInterruptSignal!ExprHandle, interruptLevel: InterruptLevel) ParseInterruptSignal!ExprHandle {
    const tracyZone = ztracy.ZoneN(@src(), "try parse binary " ++ ruleName);
    defer tracyZone.End();

    var expression = try previousRule(ctx, astgen, interruptLevel);
    while (peekOrInterrupt(ctx, interruptLevel)) |tok| {
        _ = matchTokenToExprOrNull(tok.token.tokenType, matches) orelse break;

        advance(ctx);

        const right = try previousRule(ctx, astgen, interruptLevel);

        expression = astgen.newExpression(.{ .binary = .{ .lhs = expression, .rhs = right } });
    } else |_| {}
    return expression;
}
fn orRule(ctx: Context, astgen: *AST, interruptLevel: InterruptLevel) !ExprHandle {
    const matches = &[_]TokenToBinaryExpr{.{ .key = .kwOr, .value = .bOr }};
    return binaryRule(ctx, astgen, "or", matches, andRule, interruptLevel);
}
fn andRule(ctx: Context, astgen: *AST, interruptLevel: InterruptLevel) !ExprHandle {
    const matches = &[_]TokenToBinaryExpr{.{ .key = .kwAnd, .value = .bAnd }};
    return binaryRule(ctx, astgen, "and", matches, equalityRule, interruptLevel);
}
fn equalityRule(ctx: Context, astgen: *AST, interruptLevel: InterruptLevel) !ExprHandle {
    const matches = &[_]TokenToBinaryExpr{ .{ .key = .bangEqual, .value = .notEquality }, .{ .key = .equalEqual, .value = .equality } };
    return binaryRule(ctx, astgen, "equality", matches, comparisonRule, interruptLevel);
}
fn comparisonRule(ctx: Context, astgen: *AST, interruptLevel: InterruptLevel) !ExprHandle {
    const matches = &[_]TokenToBinaryExpr{ .{ .key = .greater, .value = .greater }, .{ .key = .greaterEqual, .value = .greaterEqual }, .{ .key = .less, .value = .less }, .{ .key = .lessEqual, .value = .lessEqual } };
    return binaryRule(ctx, astgen, "comparison", matches, termRule, interruptLevel);
}
fn termRule(ctx: Context, astgen: *AST, interruptLevel: InterruptLevel) !ExprHandle {
    const matches = &[_]TokenToBinaryExpr{ .{ .key = .plus, .value = .add }, .{ .key = .minus, .value = .subtract } };
    return binaryRule(ctx, astgen, "term", matches, factorRule, interruptLevel);
}
fn factorRule(ctx: Context, astgen: *AST, interruptLevel: InterruptLevel) !ExprHandle {
    const matches = &[_]TokenToBinaryExpr{ .{ .key = .star, .value = .multiply }, .{ .key = .slash, .value = .divide }, .{ .key = .percent, .value = .modulo } };
    return binaryRule(ctx, astgen, "factor", matches, unaryRule, interruptLevel);
}
fn unaryRule(ctx: Context, astgen: *AST, interruptLevel: InterruptLevel) ParseInterruptSignal!ExprHandle {
    const tracyZone = ztracy.ZoneN(@src(), "try parse unary");
    defer tracyZone.End();

    const opToken = peekOrInterrupt(ctx, interruptLevel) catch return functionCallOrVariableRule(ctx, astgen, interruptLevel);

    _ = switch (opToken.token.tokenType) {
        .bang => .negateBool,
        .minus => .negate,
        else => return functionCallOrVariableRule(ctx, astgen, interruptLevel),
    };

    advance(ctx);

    const right = try unaryRule(ctx, astgen, interruptLevel);

    return astgen.newExpression(.{ .unary = .{ .rhs = right } });
}

// Calls and variable usages both start with an identifier, so they're combined into one rule.
fn functionCallOrVariableRule(ctx: Context, astgen: *AST, interruptLevel: InterruptLevel) ParseInterruptSignal!ExprHandle {
    const iter = ctx.tokenIterator;

    const nameCtx = peekOrInterrupt(ctx, interruptLevel) catch return primaryRule(ctx, astgen, interruptLevel);
    const nameToken = nameCtx.token;

    if (nameToken.tokenType != .identifier and nameToken.tokenType != .kwPrint) {
        return primaryRule(ctx, astgen, interruptLevel);
    }
    advance(ctx);
    const startParen = peekOrInterrupt(ctx, interruptLevel) catch {
        // same as variable
        const tracyZone = ztracy.ZoneN(@src(), "try parse variable");
        defer tracyZone.End();

        return astgen.newExpression(.{ .variable = iter.exchangeTokenForSource(nameCtx.token) });
    };

    if (startParen.token.tokenType == .leftParen) {
        const tracyZone = ztracy.ZoneN(@src(), "try parse function call");
        defer tracyZone.End();

        // Function call:
        // IDENTIFIER "(" ( expression "," )* expression? ")"
        //
        // Handled malformed inputs:
        // Statement end before closing parenthesis
        // foo(; - expected right paren,found semicolon
        //
        // Missing comma
        // foo(a b)
        // ------^ expected comma, found expression
        //
        // TODO:
        // Interrupted argument
        // foo(1 + )
        // --------^ interrupt
        advance(ctx);
        var args: [MAX_ARGS]ExprHandle = undefined;
        var argNums: usize = 0;

        // crazy nesting lol
        arguments: {
            const firstArg = peekOrInterrupt(ctx, interruptLevel) catch break :arguments;
            if (firstArg.token.tokenType == .rightParen) {
                break :arguments;
            }

            while (peekOrInterrupt(ctx, interruptLevel)) |t| {
                if (t.token.tokenType == .rightParen) {
                    // this path is only taken if we advance
                    // from a comma specifying another argument,
                    // and we encounter the right paren instead
                    ctx.log.push(.expectedExpression, t);
                    break;
                }

                const argExpr = try expressionRule(ctx, astgen, interruptLevel);
                if (argNums < MAX_ARGS) {
                    args[argNums] = argExpr;
                } else {
                    ctx.pushError(.argLimitExceeded);
                }
                argNums += 1;

                const continuation = peekOrInterrupt(ctx, interruptLevel) catch |e| {
                    if (isInterruptAtExact(e, .parenthesis)) {
                        // call (1 + )
                        // ----------^ interrupt
                        advance(ctx);
                        return NULL_HANDLE;
                    }
                    _ = filterCurrentTokenOrErr(.rightParen, ctx, interruptLevel) catch return NULL_HANDLE;
                    return astgen.newFunctionCall(iter.exchangeTokenForSource(nameCtx.token), args[0..argNums]);
                };
                switch (continuation.token.tokenType) {
                    .comma => {
                        advance(ctx);
                    },
                    .rightParen => break,
                    // If it's unrecognized, then we treat it as if it were
                    // the start of the next argument (we assume they forgot the comma).
                    else => {
                        // hacky but it works
                        _ = filterCurrentTokenOrErr(.comma, ctx, interruptLevel) catch {};
                    },
                }
            } else |_| {}
        }

        // Ending right parenthesis. If an interrupt occured (semicolon or EOF), do not advance
        // as the statement will want to use the current token.
        const end = peekOrInterrupt(ctx, interruptLevel);
        if (end) |t| {
            advance(ctx);
            if (t.token.tokenType == .rightParen) {
                return astgen.newFunctionCall(iter.exchangeTokenForSource(nameCtx.token), args[0..argNums]);
            } else {
                _ = filterCurrentTokenOrErr(.rightParen, ctx, interruptLevel) catch {};
                return NULL_HANDLE;
            }
        } else |_| {
            _ = filterCurrentTokenOrErr(.rightParen, ctx, interruptLevel) catch {};
            return NULL_HANDLE;
        }
    } else if (startParen.token.tokenType == .equal) {
        const tracyZone = ztracy.ZoneN(@src(), "try parse (invalid) assignment");
        defer tracyZone.End();

        advance(ctx);

        _ = try expressionRule(ctx, astgen, interruptLevel);
        ctx.log.push(.assignmentIsNotValidExpression, iter.peek(ctx.log));
        return NULL_HANDLE;
    } else {
        const tracyZone = ztracy.ZoneN(@src(), "try parse variable");
        defer tracyZone.End();

        return astgen.newExpression(.{ .variable = iter.exchangeTokenForSource(nameCtx.token) });
    }
}

/// RULE POSITIONAL CONTRACT:
/// If interrupted:
/// Head remains on the token causing the interrupt.
/// (1 + 2;
/// ------^ interrupt, head position
/// If successful:
/// Head is on the token after the primary.
/// 1 ...
/// ---^ head position
fn primaryRule(ctx: Context, astgen: *AST, interruptLevel: InterruptLevel) ParseInterruptSignal!ExprHandle {
    const tracyZone = ztracy.ZoneN(@src(), "try parse primary");
    defer tracyZone.End();

    const iter = ctx.tokenIterator;
    const tok = peekOrInterrupt(ctx, interruptLevel) catch |e| {
        ctx.pushError(.expectedExpression);
        return e;
    };

    const result = switch (tok.token.tokenType) {
        .leftParen => grouping: {
            advance(ctx);
            const expr = expressionRule(ctx, astgen, .parenthesis) catch |e| {
                if (isInterruptAtExact(e, .parenthesis)) {
                    // ( 1 + )
                    // ------^ interrupt here
                    advance(ctx);
                    return NULL_HANDLE;
                }
                return e;
            };

            // current will be the token following expr
            // the only other interrupt level above semicolon is right paren, which we don't want to interfere with our stuff
            const endParen = peekOrInterrupt(ctx, .semicolon) catch |e| {
                ctx.pushError(.{ .expectedToken = .{ .expected = .rightParen } });
                return e;
            };
            if (endParen.token.tokenType != .rightParen) {
                ctx.pushError(.{ .expectedToken = .{ .expected = .rightParen } });
                return NULL_HANDLE;
            }
            break :grouping expr;
        },
        .number => astgen.newExpression(.{ .literal = .{ .number = std.fmt.parseFloat(f128, iter.exchangeTokenForSource(tok.token)) catch 0 } }),
        .string => astgen.newExpression(.{ .literal = .{ .string = iter.exchangeTokenForSource(tok.token) } }),
        .kwNil => astgen.newExpression(.{ .literal = .nil }),
        .kwTrue => astgen.newExpression(.{ .literal = .true }),
        .kwFalse => astgen.newExpression(.{ .literal = .false }),
        else => {
            // Since this is the last rule checked, a rejection means there's no expression.
            // If we're calling the expression rules, we definitely need one.
            ctx.pushError(.expectedExpression);
            advance(ctx);
            return NULL_HANDLE;
        },
    };
    advance(ctx);
    return result;
}
