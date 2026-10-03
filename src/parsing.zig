const scanning = @import("scanning.zig");
const std = @import("std");
const bytecode = @import("bytecode.zig");
const ast = @import("ast.zig");
const context = @import("context.zig");
const ztracy = @import("ztracy");

// UNLESS DIRECTLY STATED OTHERWISE
// The positional contract each rule follows is:
// - If completed "successfully", the cursor lies on the token immediately after the rule.
// - If interrupted by a token, the cursor lies on the offending token.
//
// A rule is "interrupted" if it encounters certain tokens which are associated with the end of big rules.
// - A right parenthesis for the end of function arguments
// - A semicolon for the end of a line
// - A right brace for the end of blocks
// - The end of file for the end of a program
//
// The observation that lead to this was:
// Parsing rules typically expect the cursor to be on the token directly after any other rule they call.
// Example:
//
// x + 2 ;
// ------- statement
// ----- expression
// the statement expects the expression to hand control back to it with the cursor on the statement (so it can check it)
//
// But what if the semicolon comes early?
//
// x + ;
// With a naive parser, the expression rule would try to process the semicolon as an expression and then fail. With an approach
// where one error immediately cuts off parsing, this would be fine: it would probably return an error like "expected expression, found semicolon".
// With an approach where we attempt to continue parsing through errors, this would be disasterous, as the statement rule might look to the
// beginning of another statement for its semicolon, causing a cascade of extrememly unhelpful errors.

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

/// Tries to "filter" a token through a match. If no match log the error silently and return the token as usual.
/// This is a helper function to replace the if (t.tokenType != ...) in most cases. Sometimes when different paths are
/// taken based on a token this is not used.
inline fn filterCurrentToken(tt: scanning.TokenType, ctx: Context, interruptLevel: InterruptLevel) ParseInterruptSignal!Token {
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
        declarationRule(ctx, astgen, .eof) catch break;
    } else |_| {}
}

/// Token skipping contract: This function by itself does not modify the stream,
/// but it dynamically chooses a function to run based on the next token that WILL.
/// It should follow the contract of leaving
fn declarationRule(ctx: Context, astgen: *AST, interruptLevel: InterruptLevel) ParseInterruptSignal!void {
    const first = try peekOrInterrupt(ctx, .eof);

    // isn't this a bad idea? the answer is i think it is better to have this hacky approach centralized in
    // the declaration rule (kinda like in the lang spec) than have it unpredictably handled in the function declaration rule
    switch (first.token.tokenType) {
        .kwVar => try variableDeclarationRule(ctx, astgen, interruptLevel),
        .kwFun => try functionDeclarationRule(ctx, astgen),
        // this means that
        // a) there are calls and such allowed at the top of files
        // b) you can literally define a new class INSIDE a function and whatnot
        else => try statementRule(ctx, astgen, interruptLevel),
    }
}

/// This doesn't accept an interruptLevel parameter because it
/// would be really weird for this to have an interrupt level greater than eof
/// (as any early ending brace should eagerly be consumed as its own).
/// as for the others,
/// a) there's no rule where a function declaration is inside parenthesis
/// b) function declarations aren't in any statement rules either so semicolons never apply
fn functionDeclarationRule(ctx: Context, astgen: *AST) !void {
    const tracyZone = ztracy.ZoneN(@src(), "parse function declaration");
    defer tracyZone.End();

    const iter = ctx.tokenIterator;

    // fun foo ...
    // ^^^-^^^- first two
    _ = try filterCurrentToken(.kwFun, ctx, .eof);
    advance(ctx);
    const funName = try filterCurrentToken(.identifier, ctx, .eof);
    advance(ctx);

    _ = try filterCurrentToken(.leftParen, ctx, .eof);
    advance(ctx);

    var args: [MAX_ARGS][]u8 = undefined;
    var argCount: usize = 0;

    // if EOF, main ( EOF,
    // skip parsing arguments
    // should be fine to implement this hack
    const argStart = try peekOrInterrupt(ctx, .eof);
    // fun foo ( ...
    // --------^
    if (argStart.token.tokenType != .rightParen) processArgs: while (peekOrInterrupt(ctx, .eof)) |_| {
        const argName = try filterCurrentToken(.identifier, ctx, .eof);
        // fun foo ( name ...
        // ----------^^^^

        advance(ctx);
        const typeDesignatorOrInterrupt = peekOrInterrupt(ctx, .eof);
        if (typeDesignatorOrInterrupt) |typeDesignatorCtx| {
            const typeDesignator = typeDesignatorCtx.token;
            switch (typeDesignator.tokenType) {
                .comma => {
                    // ----------vvvv WHERE TYPE
                    // fun foo ( name, x ...
                    // --------------^ cursor position
                    // (DEPRECATE TYPES PLS)
                    ctx.pushError(.expectedTypeAnnotation);
                },

                .colon => {
                    // fun foo ( name : number ...
                    // ---------------^ (spaces for dramatic effect)
                    advance(ctx);
                    const argTypeOrInterrupt = peekOrInterrupt(ctx, .eof);
                    if (argTypeOrInterrupt) |argType| {
                        // fun foo ( name : <argType> ...
                        // ------------------^^^^^^^ yeah
                        switch (argType.token.tokenType) {
                            .tyNum, .tyBool, .tyString, .identifier => {
                                advance(ctx);
                            },
                            .tyVoid => {
                                ctx.pushError(.argumentTypeCannotBeVoid);
                                advance(ctx);
                            },
                            .comma => {
                                // -----------------v DO NOT ADVANCE
                                // fun foo ( name : , ...
                                ctx.pushError(.expectedTypeToken);
                            },
                            else => {
                                ctx.pushError(.expectedTypeToken);
                                advance(ctx);
                            },
                        }

                        args[argCount] = iter.exchangeTokenForSource(argName);
                    } else |err| {
                        // fun foo ( name : EOF ...
                        // -----------------^^^ (huh?)
                        ctx.pushError(.expectedTypeToken);
                        return err;
                    }
                },

                else => {
                    // fun foo ( name . ...
                    // ---------------^ (huh?)
                    // this shouldn't be a super common error, but the main case we consider is
                    // fun foo ( name. name2, name3 ), where we mistype this character for a comma
                    ctx.pushError(.expectedTypeAnnotation);
                },
            }
        } else |_| {
            // fun foo ( name EOF
        }

        const continuationOrInterrupt = peekOrInterrupt(ctx, .eof);
        if (continuationOrInterrupt) |continuation| {
            switch (continuation.token.tokenType) {
                .rightParen => {
                    // fun foo ( name )
                    // ---------------^
                    if (argCount == MAX_ARGS - 1) {
                        // TODO: rework this so that we continue parsing, but not recording arguments after the limit is reached.
                        ctx.pushError(.argLimitExceeded);
                        break;
                    } else {
                        argCount += 1;
                    }
                    break :processArgs;
                },
                .comma => {
                    // fun foo ( name, other )
                    // --------------^
                    advance(ctx);
                    if (argCount == MAX_ARGS - 1) {
                        ctx.pushError(.argLimitExceeded);
                    } else {
                        argCount += 1;
                    }
                },
                // fun foo ( name .
                // ---------------^ ??
                else => {
                    // in accordance with the previous comment we are treating it like a comma
                    advance(ctx);
                    if (argCount == MAX_ARGS - 1) {
                        ctx.pushError(.argLimitExceeded);
                    } else {
                        argCount += 1;
                    }
                },
            }
        } else |_| {}
    } else |_| {};

    // fun foo ( name, ..., nameN )
    // ---------------------------^ we need this end here
    _ = try filterCurrentToken(.rightParen, ctx, .eof);
    advance(ctx);

    // fun foo ( name ) returnType ...
    // -----------------^^^^^^^^^^
    const returnTypeTokenOrNull = peekOrInterrupt(ctx, .eof) catch |e| {
        ctx.pushError(.expectedTypeToken);
        return e;
    };
    switch (returnTypeTokenOrNull.token.tokenType) {
        .tyBool, .tyNum, .tyString, .tyVoid => {
            advance(ctx);
        },
        // Start of function body. We assume this means void.
        // -----------v
        // fun foo () { ... }
        .leftBrace => {},
        else => {
            advance(ctx);
            ctx.pushError(.expectedTypeToken);
        },
    }

    const argNames = args[0..argCount];
    // Function body
    const stmts = try blockRule(ctx, astgen);

    astgen.newFunction(ctx.tokenIterator.exchangeTokenForSource(funName), argNames, stmts);
}

/// Variable declarations are now allowed in the top level, so this takes an interrupt level arg
/// since it can be both brace interrupted (block) and eof interrupted (top-level).
/// This needs to process its own semicolon.
fn variableDeclarationRule(ctx: Context, astgen: *AST, interruptLevel: InterruptLevel) ParseInterruptSignal!void {
    const tracyZone = ztracy.ZoneN(@src(), "try parse variable declaration");
    defer tracyZone.End();

    const iter = ctx.tokenIterator;
    const decl = try peekOrInterrupt(ctx, interruptLevel);

    // this is fed by the declaration rule so this should NEVER run
    if (decl.token.tokenType != .kwVar) {
        unreachable;
    }
    advance(ctx);

    // var foo ...
    // ^^^-^^^
    const nameToken = try peekOrInterrupt(ctx, interruptLevel);
    if (nameToken.token.tokenType != .identifier) {
        ctx.pushError(.{ .expectedToken = .{ .expected = .identifier } });
    }

    advance(ctx);
    const typeHintOrEqualsCtx = peekOrInterrupt(ctx, interruptLevel) catch |e| {
        // var foo EOF - we never got a semicolon :(
        ctx.pushError(.expectedTypeAnnotation);
        return e;
    };
    const typeHintOrEquals = typeHintOrEqualsCtx.token;
    switch (typeHintOrEquals.tokenType) {
        .colon => {
            advance(ctx);

            // var foo : typeToken
            // ----------^^^^^^^^^
            const typeToken = peekOrInterrupt(ctx, interruptLevel) catch |e| {
                // var foo : EOF
                ctx.pushError(.expectedTypeToken);
                ctx.pushError(.{ .expectedToken = .{ .expected = .semicolon } });
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

            // var foo : typeToken
            // ----------^^^^^^^^^
            const nextCtx = try peekOrInterrupt(ctx, interruptLevel);
            const next = nextCtx.token;
            const initialValue: ExprHandle = val: {
                switch (next.tokenType) {
                    // var foo : typeToken = ...
                    // --------------------^
                    .equal => {
                        advance(ctx);
                        break :val try expressionRule(ctx, astgen, interruptLevel);
                    },
                    // var foo : typeToken ;
                    // --------------------^ in the event this is not a semicolon it will be caught
                    else => {
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
            // var foo = ...
            // --------^ rudimentary type inference
            advance(ctx);
            _ = astgen.newStatement(.{
                .declaration = .{
                    .name = iter.exchangeTokenForSource(nameToken.token),
                    .val = try expressionRule(ctx, astgen, interruptLevel),
                },
            });
        },
        .semicolon => {
            // var foo; - this is technically allowed but we're stripping out types l8r
            // -------^
            ctx.pushError(.expectedTypeAnnotation);
            advance(ctx);
        },
        else => {
            // var foo . - we need a semicolon here
            // --------^ we won't advance here, instead leave it
            // the next statement scan will go down to primary and mark it as
            // unrecognized then advance itself
            ctx.pushError(.{ .expectedToken = .{ .expected = .semicolon } });
        },
    }

    // var foo (...) ; <-- if this isn't a semicolon, we're still at the end of the line
    _ = try filterCurrentToken(.semicolon, ctx, interruptLevel);
    advance(ctx);
}

fn blockRule(ctx: Context, astgen: *AST) !StmtRange {
    const tracyZone = ztracy.ZoneN(@src(), "parse code block");
    defer tracyZone.End();

    // this should always be leftBrace since it's fed via the statement rule
    _ = try filterCurrentToken(.leftBrace, ctx, .eof);
    advance(ctx);

    const retInfo = try blockBodyRule(ctx, astgen);

    _ = try filterCurrentToken(.rightBrace, ctx, .eof);
    advance(ctx);
    return retInfo;
}

fn blockBodyRule(ctx: Context, astgen: *AST) !StmtRange {
    const stmtStart: u32 = @truncate(astgen.statementList.items.len);

    stmts: while (peekOrInterrupt(ctx, .brace)) |_| {
        declarationRule(ctx, astgen, .brace) catch |e| {
            if (isInterruptAtExact(e, .eof)) {
                return e;
            } else {
                // must be a brace interrupt because we specified brace level or above
                // this means our body is done
                break :stmts;
            }
        };
    } else |_| {}
    // when we are interrupted by either EOF or right brace
    const stmtEnd: u32 = @truncate(astgen.statementList.items.len);
    _ = filterCurrentToken(.rightBrace, ctx, .eof) catch {};
    return .{ .start = stmtStart, .end = stmtEnd };
}

fn ifRule(ctx: Context, astgen: *AST) !void {
    const tracyZone = ztracy.ZoneN(@src(), "parse if statement");
    defer tracyZone.End();

    // should never error due to this being fed by statement rule
    _ = try filterCurrentToken(.kwIf, ctx, .brace);
    advance(ctx);

    // if (
    // ---^ because if statements have a statement body (i.e. if (foo) return bar; is valid),
    //      a semicolon here means we're cooked
    _ = filterCurrentToken(.leftParen, ctx, .semicolon) catch |e| {
        // we expected a conditional and got a break
        ctx.pushError(.expectedExpression);
        return e;
    };
    advance(ctx);

    // if ( ...
    // -----^^^ pos
    // use the result later

    // if ( expr ;
    // ----------^ because if statements have a statement body (i.e. if (foo) return bar; is valid),
    //            a semicolon here means we're cooked
    // if ( expr )
    //           ^ position will be left here assuming things go ok
    _ = try expressionRule(ctx, astgen, .semicolon);

    // if ( expr )
    // ----------^
    // we are forced to accept semicolon breaks for the same reason
    _ = try filterCurrentToken(.rightParen, ctx, .semicolon);
    advance(ctx);

    // if ( expr ) body
    // ------------^^^^
    // this uses the brace interrupt level as statements can accept semicolons
    try statementRule(ctx, astgen, .brace);

    // if ( expr ) body else?
    // -----------------^^^
    // this is the bridge to an optional part, so we shouldn't be returning interrupts (it should be treated as the next line)
    const possibleElse = peekOrInterrupt(ctx, .semicolon) catch return;
    if (possibleElse.token.tokenType != .kwElse) {
        // an example of when this might happen:
        // if ( expr ) foo(); bar();
        // -------------------^^^ where the cursor would be
        return;
    }

    // if ( expr ) ... else ...
    // ---------------------^^^
    advance(ctx);
    // this uses the brace interrupt level as statements can accept semicolons
    try statementRule(ctx, astgen, .brace);
}

/// This needs either brace interrupt level or EOF level.
fn statementRule(ctx: Context, astgen: *AST, interruptLevel: InterruptLevel) ParseInterruptSignal!void {
    const tracyZone = ztracy.ZoneN(@src(), "parse statement");
    defer tracyZone.End();
    const firstToken = try peekOrInterrupt(ctx, .semicolon);

    // again, i think this is the best way to express how the statement rule branches into 3 different other rules
    switch (firstToken.token.tokenType) {
        .leftBrace => _ = try blockRule(ctx, astgen),
        .kwReturn => _ = try returnRule(ctx, astgen, interruptLevel),
        .kwIf => _ = try ifRule(ctx, astgen),
        else => _ = try expressionStatementRule(ctx, astgen, interruptLevel),
    }
}

/// interruptLevel should be either brace or EOF.
fn expressionStatementRule(ctx: Context, astgen: *AST, interruptLevel: InterruptLevel) !void {
    _ = expressionRule(ctx, astgen, .semicolon) catch |interrupt| {
        if (isInterruptAtOrAbove(interrupt, interruptLevel)) {
            return interrupt;
        } else if (isInterruptAtExact(interrupt, .semicolon)) {
            advance(ctx);
            return;
        } else {
            // the only case where this might happen is a brace interrupt while this has EOF interrupt as of now
            _ = try filterCurrentToken(.semicolon, ctx, interruptLevel);
            advance(ctx);
            return;
        }
    };

    const sc = try filterCurrentToken(.semicolon, ctx, interruptLevel);
    // if a semicolon ends the line, eg (1 + 2);, then advance as is the positional contract
    // else, eg (1 + 2)a, assume a missing semicolon and that this token is the start of the next line
    if (sc.tokenType == .semicolon) {
        advance(ctx);
    }
}

/// The grammar expects this to process its own semicolon.
/// Similar to expr statements, this can either be interrupted by braces or eof, so this accepts that parameter.
fn returnRule(ctx: Context, astgen: *AST, interruptLevel: InterruptLevel) !void {
    const tracyZone = ztracy.ZoneN(@src(), "try parse return");
    defer tracyZone.End();

    const ret = try peekOrInterrupt(ctx, interruptLevel);
    if (ret.token.tokenType != .kwReturn) {
        // should never happen because this is fed by the statement rule
        unreachable;
    }

    // return (...)
    // -------^^^^^ pos after this advance call
    advance(ctx);

    // There are 4 typical cases that arise from this.
    // 1. return (expr) ; - Note that we don't care whether expr is valid for parsing purposes.
    // 2. return ;
    // 3. return (expr) EOF - The following two cases are syntax errors.
    // 4. return EOF

    if (peekOrInterrupt(ctx, .semicolon)) |_| {
        const val = expressionRule(ctx, astgen, .semicolon) catch |e| {
            if (isInterruptAtExact(e, .semicolon)) {
                // basically the same case 1, this rule doesn't care if the expression is malformed or not
                advance(ctx);
                return;
            }
            // case 3
            _ = filterCurrentToken(.semicolon, ctx, interruptLevel) catch {};
            return e;
        };

        _ = astgen.newStatement(.{ .funReturn = val });
        // return (expr) ; <- case 1
        _ = try filterCurrentToken(.semicolon, ctx, interruptLevel);
    } else |e| {
        if (isInterruptAtExact(e, .semicolon)) {
            // case 2
            advance(ctx);
            return;
        }
        // case 4
        _ = filterCurrentToken(.semicolon, ctx, interruptLevel) catch {};
        return e;
    }
}

fn expressionRule(ctx: Context, astgen: *AST, interruptLevel: InterruptLevel) ParseInterruptSignal!ExprHandle {
    const tracyZone = ztracy.ZoneN(@src(), "try parse expression");
    defer tracyZone.End();

    _ = peekOrInterrupt(ctx, interruptLevel) catch {
        ctx.pushError(.expectedExpression);
        return NULL_HANDLE;
    };
    return try assignmentRule(ctx, astgen, interruptLevel);
}

fn assignmentRule(ctx: Context, astgen: *AST, interruptLevel: InterruptLevel) !ExprHandle {
    const tracyZone = ztracy.ZoneN(@src(), "try parse assignment or fallthrough");
    defer tracyZone.End();

    // this is hacky but whatever!!
    const prevPosition = ctx.tokenIterator.*;

    // foo = ...
    // ^^^----
    const name = try peekOrInterrupt(ctx, interruptLevel);
    if (name.token.tokenType != .identifier) {
        // assignment = (... | logic_or)
        return try orRule(ctx, astgen, interruptLevel);
    }

    advance(ctx);

    // foo = ...
    // ----^
    const eq = try peekOrInterrupt(ctx, interruptLevel);
    if (eq.token.tokenType != .equal) {
        // foo ...
        // ----^^^ we need to return things to how they were before we fallthrough to the next rule
        ctx.tokenIterator.* = prevPosition;
        return try orRule(ctx, astgen, interruptLevel);
    }

    advance(ctx);

    // foo = ...
    // ------^^^
    const val = try expressionRule(ctx, astgen, interruptLevel);
    _ = astgen.newStatement(.{
        .assignment = .{
            .name = ctx.tokenIterator.exchangeTokenForSource(name.token),
            .val = val,
        },
    });

    // Per the langauge spec in chapter 8.4.2, an assignment expression returns the newly assigned value.
    return val;
}

// might be the most atrocious function body i've ever written
/// RULE POSITIONAL CONTRACT:
/// If interrupted:
/// Head remains on the token causing the interrupt.
/// (1 + 2;
/// ------^ interrupt, head position
/// If successful:
/// Head is on the token after the primary.
/// 1 ...
/// ---^ head position
inline fn binaryRule(ctx: Context, astgen: *AST, comptime ruleName: [:0]const u8, comptime matches: []const TokenToBinaryExpr, previousRule: fn (Context, *AST, InterruptLevel) ParseInterruptSignal!ExprHandle, interruptLevel: InterruptLevel) ParseInterruptSignal!ExprHandle {
    const tracyZone = ztracy.ZoneN(@src(), "try parse binary " ++ ruleName);
    defer tracyZone.End();

    var expression = try previousRule(ctx, astgen, interruptLevel);
    while (peekOrInterrupt(ctx, interruptLevel)) |tok| {
        // 5 * 5 +
        //
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
/// RULE POSITIONAL CONTRACT:
/// If interrupted:
/// Head remains on the token causing the interrupt.
/// (1 + 2;
/// ------^ interrupt, head position
/// If successful:
/// Head is on the token after the primary.
/// 1 ...
/// ---^ head position
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
                    _ = filterCurrentToken(.rightParen, ctx, interruptLevel) catch return NULL_HANDLE;
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
                        _ = filterCurrentToken(.comma, ctx, interruptLevel) catch {};
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
                _ = filterCurrentToken(.rightParen, ctx, interruptLevel) catch {};
                return NULL_HANDLE;
            }
        } else |_| {
            _ = filterCurrentToken(.rightParen, ctx, interruptLevel) catch {};
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
