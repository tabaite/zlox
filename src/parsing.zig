const scanning = @import("scanning.zig");
const std = @import("std");
const bytecode = @import("bytecode.zig");
const context = @import("context.zig");
const ztracy = @import("ztracy");

const MAX_ARGS = bytecode.MAX_ARGS;
const Token = scanning.Token;
const TokenContext = scanning.TokenContext;
const CodeGen = bytecode.BytecodeGenerator;
const Allocator = std.mem.Allocator;
const AnyWriter = std.io.AnyWriter;
const Context = context.Context;

const Handle = bytecode.HandledOperand;

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
    ReachedEndOfStatement,
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
    const tracyZone = ztracy.ZoneN(@src(), "peek token stream or interrupt");
    defer tracyZone.End();

    const tokenContext = ctx.tokenIterator.peek(ctx.log);
    const token = tokenContext.token;
    const MatchVec = @Vector(4, u32);
    const TT = scanning.TokenType;

    const interruptMatches: [4]MatchVec = .{
        .{ @intFromEnum(TT.eof), 0, 0, 0 },
        .{ @intFromEnum(TT.eof), @intFromEnum(TT.rightBrace), 0, 0 },
        .{ @intFromEnum(TT.eof), @intFromEnum(TT.rightBrace), @intFromEnum(TT.semicolon), 0 },
        .{ @intFromEnum(TT.eof), @intFromEnum(TT.rightBrace), @intFromEnum(TT.semicolon), @intFromEnum(TT.rightParen) },
    };

    const mask: MatchVec = @splat(@intFromEnum(token.tokenType));
    const interruptResult = interruptMatches[level.asInt()] == mask;
    return if (@reduce(.Or, interruptResult)) ParseInterruptSignal.ReachedEndOfStatement else tokenContext;
}

inline fn advance(ctx: Context) void {
    _ = ctx.tokenIterator.next(ctx.log);
}

const TokenFilterError = error{
    DoesNotMatch,
};

// Tries to "filter" a token through a match. If no match, return null, and log the error.
inline fn filterCurrentTokenOrErr(tt: scanning.TokenType, ctx: Context, interruptLevel: InterruptLevel) !Token {
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
pub fn parseAndCompileAll(ctx: Context, codegen: *CodeGen) void {
    const parseZone = ztracy.ZoneN(@src(), "parse + compile");
    defer parseZone.End();

    while (peekOrInterrupt(ctx, .eof)) |_| {
        functionDeclarationRule(ctx, codegen);
    } else |_| {}
    // If there is no active function, this is a no-op.
    // Otherwise (if the function has not ended by eof) this prevents a nasty bug.
    codegen.exitFunction(ctx);
}

fn functionDeclarationRule(ctx: Context, codegen: *CodeGen) void {
    const tracyZone = ztracy.ZoneN(@src(), "parse function declaration");
    defer tracyZone.End();

    const iter = ctx.tokenIterator;
    _ = filterCurrentTokenOrErr(.kwFun, ctx, .eof) catch {};
    advance(ctx);

    const funNameTOrErr = filterCurrentTokenOrErr(.identifier, ctx, .eof);
    advance(ctx);

    _ = filterCurrentTokenOrErr(.leftParen, ctx, .eof) catch {};
    advance(ctx);

    var args: [MAX_ARGS]bytecode.ArgInfo = undefined;
    var argCount: usize = 0;

    // if EOF, main ( EOF,
    // skip parsing arguments
    // should be fine to implement this hack
    const argStart = peekOrInterrupt(ctx, .eof) catch TokenContext{ .newPos = 0, .lineNumber = 0, .token = .{ .tokenType = .rightParen, .sourceEndExclusive = 0, .sourceStart = 0 } };
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
                    if (argTypeOrInterrupt) |argType| {
                        args[argCount] = .{
                            .type = switch (argType.token.tokenType) {
                                .tyBool => .bool,
                                .tyNum => .number,
                                .tyString => .string,
                                .tyVoid => e: {
                                    ctx.pushError(.argumentTypeCannotBeVoid);
                                    break :e .nil;
                                },
                                else => e: {
                                    ctx.pushError(.expectedTypeToken);
                                    break :e .nil;
                                },
                            },
                            .name = iter.exchangeTokenForSource(argName),
                        };
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
    const retType: bytecode.Type = ret: switch (returnTypeTokenOrNull.token.tokenType) {
        .tyBool => {
            advance(ctx);
            break :ret .bool;
        },
        .tyNum => {
            advance(ctx);
            break :ret .number;
        },
        .tyString => {
            advance(ctx);
            break :ret .string;
        },
        .tyVoid => {
            advance(ctx);
            break :ret .nil;
        },
        // Start of function body. We assume this means void.
        //         v
        .leftBrace => break :ret .nil,
        else => {
            advance(ctx);
            ctx.pushError(.expectedTypeToken);
            break :ret .nil;
        },
    };
    if (funNameTOrErr) |funNameT| {
        const funName = iter.exchangeTokenForSource(funNameT);
        codegen.enterFunction(ctx, funName, args[0..argCount], retType);
    } else |_| {}
    // Function body
    _ = blockRule(ctx, codegen);
    // works even if we don't enter the function
    codegen.exitFunction(ctx);
}

fn blockRule(ctx: Context, codegen: *CodeGen) BlockReturnInfo {
    const tracyZone = ztracy.ZoneN(@src(), "parse block");
    defer tracyZone.End();

    const defaultRetInfo: BlockReturnInfo = .{ .returnsOnAllPaths = true };
    _ = filterCurrentTokenOrErr(.leftBrace, ctx, .eof) catch return defaultRetInfo;
    advance(ctx);

    codegen.enterScope();
    const retInfo = blockBodyRule(ctx, codegen);
    codegen.exitScope();

    _ = filterCurrentTokenOrErr(.rightBrace, ctx, .eof) catch return retInfo;
    advance(ctx);
    return retInfo;
}

fn blockBodyRule(ctx: Context, codegen: *CodeGen) BlockReturnInfo {
    const tracyZone = ztracy.ZoneN(@src(), "parse block body");
    defer tracyZone.End();

    var blockRetInfo: BlockReturnInfo = .{ .returnsOnAllPaths = false };
    while (peekOrInterrupt(ctx, .brace)) |t| {
        const tk = t.token;
        const subblockRetInfo: BlockReturnInfo = switch (tk.tokenType) {
            .leftBrace => blockRule(ctx, codegen),
            // might not be reachable..?
            .rightBrace => return blockRetInfo,
            else => statementRule(ctx, codegen) catch break,
        };
        blockRetInfo.returnsOnAllPaths = blockRetInfo.returnsOnAllPaths or subblockRetInfo.returnsOnAllPaths;
    } else |_| {}
    // when we are interrupted by either EOF or right brace
    _ = filterCurrentTokenOrErr(.rightBrace, ctx, .eof) catch {};
    return blockRetInfo;
}

fn statementRule(ctx: Context, codegen: *CodeGen) !BlockReturnInfo {
    const tracyZone = ztracy.ZoneN(@src(), "parse statement");
    defer tracyZone.End();

    const blockRetInfo = assignmentRule(ctx, codegen);

    const semicolonMatchOrErr = filterCurrentTokenOrErr(.semicolon, ctx, .brace);
    if (semicolonMatchOrErr) |_| {
        advance(ctx);
    } else |err| {
        switch (err) {
            ParseInterruptSignal.ReachedEndOfStatement => return err,
            TokenFilterError.DoesNotMatch => return blockRetInfo catch .{ .returnsOnAllPaths = true },
        }
    }
    return blockRetInfo catch .{ .returnsOnAllPaths = true };
}

fn assignmentRule(ctx: Context, codegen: *CodeGen) !BlockReturnInfo {
    const tracyZone = ztracy.ZoneN(@src(), "try parse assignment or fallthrough");
    defer tracyZone.End();

    const prevPosition = ctx.tokenIterator.*;

    const name = try peekOrInterrupt(ctx, .semicolon);
    if (name.token.tokenType != .identifier) {
        return try returnRule(ctx, codegen);
    }

    advance(ctx);

    const eq = try peekOrInterrupt(ctx, .semicolon);
    if (eq.token.tokenType != .equal) {
        ctx.tokenIterator.* = prevPosition;
        return try returnRule(ctx, codegen);
    }

    advance(ctx);

    const val = try expressionRule(ctx, codegen, .semicolon);
    _ = codegen.updateVariable(ctx, ctx.tokenIterator.exchangeTokenForSource(name.token), val);
    return .{ .returnsOnAllPaths = false };
}

fn returnRule(ctx: Context, codegen: *CodeGen) !BlockReturnInfo {
    const tracyZone = ztracy.ZoneN(@src(), "try parse return or fallthrough");
    defer tracyZone.End();

    const ret = try peekOrInterrupt(ctx, .semicolon);
    if (ret.token.tokenType != .kwReturn) {
        try declarationRule(ctx, codegen);
        return .{ .returnsOnAllPaths = false };
    }
    advance(ctx);
    if (peekOrInterrupt(ctx, .semicolon)) |_| {
        codegen.insertFunctionReturn(ctx, try expressionRule(ctx, codegen, .semicolon));
    } else |_| {}
    return .{ .returnsOnAllPaths = true };
}

fn declarationRule(ctx: Context, codegen: *CodeGen) ParseInterruptSignal!void {
    const tracyZone = ztracy.ZoneN(@src(), "try parse declaration or fallthrough");
    defer tracyZone.End();

    const iter = ctx.tokenIterator;
    const decl = try peekOrInterrupt(ctx, .semicolon);
    if (decl.token.tokenType != .kwVar) {
        _ = try expressionRule(ctx, codegen, .semicolon);
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
        return ParseInterruptSignal.ReachedEndOfStatement;
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
            const varType: bytecode.Type = switch (typeToken.token.tokenType) {
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
            const initialValue: ?Handle = val: {
                switch (next.tokenType) {
                    .equal => {
                        advance(ctx);
                        break :val try expressionRule(ctx, codegen, .semicolon);
                    },
                    .semicolon => {
                        break :val null;
                    },
                    else => {
                        ctx.pushError(.{ .expectedToken = .{ .expected = .semicolon } });
                        break :val null;
                    },
                }
            };

            _ = codegen.registerVariable(ctx, iter.exchangeTokenForSource(nameToken.token), .{ .provided = .{ .type = varType, .initial = initialValue } });
            return;
        },
        .equal => {
            advance(ctx);
            _ = codegen.registerVariable(ctx, iter.exchangeTokenForSource(nameToken.token), .{ .fromValue = try expressionRule(ctx, codegen, .semicolon) });
            return;
        },
        // Includes semicolon.
        else => {
            ctx.pushError(.expectedTypeAnnotation);
            return;
        },
    }
}

fn expressionRule(ctx: Context, codegen: *CodeGen, interruptLevel: InterruptLevel) ParseInterruptSignal!Handle {
    const tracyZone = ztracy.ZoneN(@src(), "try parse expression");
    defer tracyZone.End();

    _ = peekOrInterrupt(ctx, interruptLevel) catch {
        ctx.pushError(.expectedExpression);
        return .ERR;
    };
    return try orRule(ctx, codegen, interruptLevel);
}

// might be the most atrocious function body i've ever written
inline fn binaryRule(ctx: Context, codegen: *CodeGen, comptime ruleName: [:0]const u8, comptime matches: []const TokenToBinaryExpr, previousRule: fn (Context, *CodeGen, InterruptLevel) ParseInterruptSignal!Handle, interruptLevel: InterruptLevel) ParseInterruptSignal!Handle {
    const tracyZone = ztracy.ZoneN(@src(), "try parse binary " ++ ruleName);
    defer tracyZone.End();

    var expression = try previousRule(ctx, codegen, interruptLevel);
    while (peekOrInterrupt(ctx, interruptLevel)) |tok| {
        const operation = matchTokenToExprOrNull(tok.token.tokenType, matches) orelse break;

        advance(ctx);

        const right = try previousRule(ctx, codegen, interruptLevel);

        expression = codegen.pushBinaryOperation(ctx, operation, expression, right);
    } else |_| {}
    return expression;
}
fn orRule(ctx: Context, codegen: *CodeGen, interruptLevel: InterruptLevel) !Handle {
    const matches = &[_]TokenToBinaryExpr{.{ .key = .kwOr, .value = .bOr }};
    return binaryRule(ctx, codegen, "or", matches, andRule, interruptLevel);
}
fn andRule(ctx: Context, codegen: *CodeGen, interruptLevel: InterruptLevel) !Handle {
    const matches = &[_]TokenToBinaryExpr{.{ .key = .kwAnd, .value = .bAnd }};
    return binaryRule(ctx, codegen, "and", matches, equalityRule, interruptLevel);
}
fn equalityRule(ctx: Context, codegen: *CodeGen, interruptLevel: InterruptLevel) !Handle {
    const matches = &[_]TokenToBinaryExpr{ .{ .key = .bangEqual, .value = .notEquality }, .{ .key = .equalEqual, .value = .equality } };
    return binaryRule(ctx, codegen, "equality", matches, comparisonRule, interruptLevel);
}
fn comparisonRule(ctx: Context, codegen: *CodeGen, interruptLevel: InterruptLevel) !Handle {
    const matches = &[_]TokenToBinaryExpr{ .{ .key = .greater, .value = .greater }, .{ .key = .greaterEqual, .value = .greaterEqual }, .{ .key = .less, .value = .less }, .{ .key = .lessEqual, .value = .lessEqual } };
    return binaryRule(ctx, codegen, "comparison", matches, termRule, interruptLevel);
}
fn termRule(ctx: Context, codegen: *CodeGen, interruptLevel: InterruptLevel) !Handle {
    const matches = &[_]TokenToBinaryExpr{ .{ .key = .plus, .value = .add }, .{ .key = .minus, .value = .subtract } };
    return binaryRule(ctx, codegen, "term", matches, factorRule, interruptLevel);
}
fn factorRule(ctx: Context, codegen: *CodeGen, interruptLevel: InterruptLevel) !Handle {
    const matches = &[_]TokenToBinaryExpr{ .{ .key = .star, .value = .multiply }, .{ .key = .slash, .value = .divide }, .{ .key = .percent, .value = .modulo } };
    return binaryRule(ctx, codegen, "factor", matches, unaryRule, interruptLevel);
}
fn unaryRule(ctx: Context, codegen: *CodeGen, interruptLevel: InterruptLevel) ParseInterruptSignal!Handle {
    const tracyZone = ztracy.ZoneN(@src(), "try parse unary");
    defer tracyZone.End();

    const opToken = peekOrInterrupt(ctx, interruptLevel) catch return functionCallOrVariableOrAssignmentRule(ctx, codegen, interruptLevel);

    const operation: UnaryExprType = switch (opToken.token.tokenType) {
        .bang => .negateBool,
        .minus => .negate,
        else => return functionCallOrVariableOrAssignmentRule(ctx, codegen, interruptLevel),
    };

    advance(ctx);

    const right = try unaryRule(ctx, codegen, interruptLevel);

    return codegen.pushUnaryOperation(ctx, operation, right);
}

// Calls and variable usages both start with an identifier, so they're combined into one rule.
fn functionCallOrVariableOrAssignmentRule(ctx: Context, codegen: *CodeGen, interruptLevel: InterruptLevel) ParseInterruptSignal!Handle {
    const iter = ctx.tokenIterator;

    const nameCtx = peekOrInterrupt(ctx, interruptLevel) catch return primaryRule(ctx, codegen, interruptLevel);
    const nameToken = nameCtx.token;

    if (nameToken.tokenType != .identifier and nameToken.tokenType != .kwPrint) {
        return primaryRule(ctx, codegen, interruptLevel);
    }
    advance(ctx);
    const startParen = peekOrInterrupt(ctx, interruptLevel) catch {
        // same as variable
        const tracyZone = ztracy.ZoneN(@src(), "try parse variable");
        defer tracyZone.End();

        return codegen.getVariable(ctx, iter.exchangeTokenForSource(nameCtx.token));
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
        advance(ctx);
        var args: [MAX_ARGS]Handle = undefined;
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

                const argExpr = try expressionRule(ctx, codegen, interruptLevel);
                if (argNums < MAX_ARGS) {
                    args[argNums] = argExpr;
                } else {
                    ctx.pushError(.argLimitExceeded);
                }
                argNums += 1;

                const continuation = peekOrInterrupt(ctx, interruptLevel) catch {
                    _ = filterCurrentTokenOrErr(.rightParen, ctx, interruptLevel) catch return .ERR;
                    return codegen.callFunction(ctx, iter.exchangeTokenForSource(nameCtx.token), args[0..argNums]);
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
                return codegen.callFunction(ctx, iter.exchangeTokenForSource(nameCtx.token), args[0..argNums]);
            } else {
                _ = filterCurrentTokenOrErr(.rightParen, ctx, interruptLevel) catch {};
                return .ERR;
            }
        } else |_| {
            _ = filterCurrentTokenOrErr(.rightParen, ctx, interruptLevel) catch {};
            return .ERR;
        }
    } else if (startParen.token.tokenType == .equal) {
        const tracyZone = ztracy.ZoneN(@src(), "try parse (invalid) assignment");
        defer tracyZone.End();

        advance(ctx);

        _ = try expressionRule(ctx, codegen, interruptLevel);
        ctx.log.push(.assignmentIsNotValidExpression, iter.peek(ctx.log));
        return .ERR;
    } else {
        const tracyZone = ztracy.ZoneN(@src(), "try parse variable");
        defer tracyZone.End();

        return codegen.getVariable(ctx, iter.exchangeTokenForSource(nameCtx.token));
    }
}

fn primaryRule(ctx: Context, codegen: *CodeGen, interruptLevel: InterruptLevel) ParseInterruptSignal!Handle {
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
            const expr = try expressionRule(ctx, codegen, .parenthesis);

            // current will be the token following expr
            // the only other interrupt level above semicolon is right paren, which we don't want to interfere with our stuff
            const endParen = peekOrInterrupt(ctx, .semicolon) catch |e| {
                ctx.pushError(.{ .expectedToken = .{ .expected = .rightParen } });
                return e;
            };
            if (endParen.token.tokenType != .rightParen) {
                ctx.pushError(.{ .expectedToken = .{ .expected = .rightParen } });
                return .ERR;
            }
            break :grouping expr;
        },
        .number => CodeGen.newNumberLit(std.fmt.parseFloat(f64, iter.exchangeTokenForSource(tok.token)) catch 0),
        .string => codegen.newStringLit(iter.exchangeTokenForSource(tok.token)),
        .kwNil => CodeGen.newNilLit(),
        .kwTrue => comptime CodeGen.newBoolLit(true),
        .kwFalse => comptime CodeGen.newBoolLit(false),
        else => {
            // Since this is the last rule checked, a rejection means there's no expression.
            // If we're calling the expression rules, we definitely need one.
            ctx.pushError(.expectedExpression);
            advance(ctx);
            return .ERR;
        },
    };
    advance(ctx);
    return result;
}
