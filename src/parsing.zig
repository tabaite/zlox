const scanning = @import("scanning.zig");
const std = @import("std");
const bytecode = @import("bytecode.zig");
const context = @import("context.zig");

const MAX_ARGS = bytecode.MAX_ARGS;
const Token = scanning.Token;
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

// HELPERS
inline fn matchTokenToExprOrNull(target: scanning.TokenType, comptime matches: []const TokenToBinaryExpr) ?BinaryExprType {
    inline for (matches) |t| {
        if (t.key == target) {
            return t.value;
        }
    }
    return null;
}

/// returns an interrupt if the token is a semicolon or the eof.
inline fn peekOrInterrupt(ctx: Context) ParseInterruptSignal!Token {
    const t = ctx.tokenIterator.peek(ctx.log) orelse return ParseInterruptSignal.ReachedEndOfStatement;
    if (t.tokenType == .semicolon) {
        return ParseInterruptSignal.ReachedEndOfStatement;
    }
    return t;
}

inline fn peek(ctx: Context) ?Token {
    return ctx.tokenIterator.peek(ctx.log);
}

inline fn advance(ctx: Context) void {
    _ = ctx.tokenIterator.next(ctx.log);
}

/// Token or Invalid
inline fn toi(t: ?Token) Token {
    return t orelse .{ .sourceStart = 0, .sourceEndExclusive = 0, .tokenType = .invalidChar };
}

// Tries to "filter" a token through a match. If no match, return null, and log the error.
inline fn filterCurrentTokenOrErr(tt: scanning.TokenType, ctx: Context) ?Token {
    const log = ctx.log;
    const iter = ctx.tokenIterator;
    const token = iter.peek(log);
    if (token) |t| {
        if (t.tokenType != tt) {
            log.push(.{ .expectedToken = .{ .expected = tt, .found = t } }, iter.getCurrentTokenContext());
            return null;
        }
        return t;
    } else {
        log.push(.{ .expectedToken = .{ .expected = tt, .found = null } }, iter.getCurrentTokenContext());
        return null;
    }
}

// The way this AST parser works is somewhat simple.
// Each rule, described by the table above is a function.
// The function mutates the state of the parser, moving the position forward
// to the token immediately after the expression it returns.
pub fn parseAndCompileAll(ctx: Context, codegen: *CodeGen) void {
    const iter = ctx.tokenIterator;
    const log = ctx.log;
    while (iter.peek(log)) |_| {
        functionDeclarationRule(ctx, codegen);
    }
    // If there is no active function, this is a no-op.
    // Otherwise (if the function has not ended by eof) this prevents a nasty bug.
    codegen.exitFunction(ctx);
}

fn functionDeclarationRule(ctx: Context, codegen: *CodeGen) void {
    const iter = ctx.tokenIterator;
    _ = filterCurrentTokenOrErr(.kwFun, ctx);
    advance(ctx);

    const funNameTOrNull = filterCurrentTokenOrErr(.identifier, ctx);
    advance(ctx);

    _ = filterCurrentTokenOrErr(.leftParen, ctx);
    advance(ctx);

    var args: [MAX_ARGS]bytecode.ArgInfo = undefined;
    var argCount: usize = 0;

    // if EOF, main ( EOF,
    // skip parsing arguments
    const argStart: Token = peek(ctx) orelse .{ .tokenType = .rightParen, .sourceEndExclusive = 0, .sourceStart = 0 };

    if (argStart.tokenType != .rightParen) while (peek(ctx)) |_| {
        const arg_name = filterCurrentTokenOrErr(.identifier, ctx) orelse break;
        advance(ctx);
        const typeDesignatorOrNull = peek(ctx);
        if (typeDesignatorOrNull) |typeDesignator| {
            switch (typeDesignator.tokenType) {
                .comma => {
                    ctx.pushError(.expectedTypeAnnotation);
                },

                .colon => {
                    advance(ctx);
                    const arg_type_or_null = peek(ctx);
                    if (arg_type_or_null) |arg_type| {
                        args[argCount] = .{
                            .type = switch (arg_type.tokenType) {
                                .tyBool => .bool,
                                .tyNum => .number,
                                .tyString => .string,
                                .tyVoid => e: {
                                    ctx.pushError(.argumentTypeCannotBeVoid);
                                    break :e .nil;
                                },
                                else => e: {
                                    ctx.pushError(.{ .expectedTypeToken = .{ .found = arg_type } });
                                    break :e .nil;
                                },
                            },
                            .name = iter.exchangeTokenForSource(arg_name),
                        };
                    } else {
                        ctx.pushError(.{ .expectedTypeToken = .{ .found = null } });
                    }
                    advance(ctx);
                },

                else => {
                    ctx.pushError(.expectedTypeAnnotation);
                    break;
                },
            }
        }

        const continuation = peek(ctx);
        switch (toi(continuation).tokenType) {
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
            else => ctx.pushError(.{ .expectedToken = .{ .expected = .comma, .found = continuation } }),
        }
    };

    _ = filterCurrentTokenOrErr(.rightParen, ctx);
    advance(ctx);

    const returnTypeTokenOrNull = peek(ctx);
    const retType: bytecode.Type = ret: switch (toi(returnTypeTokenOrNull).tokenType) {
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
            ctx.pushError(.{ .expectedTypeToken = .{ .found = returnTypeTokenOrNull } });
            break :ret .nil;
        },
    };
    if (funNameTOrNull) |funNameT| {
        const funName = iter.exchangeTokenForSource(funNameT);
        codegen.enterFunction(ctx, funName, args[0..argCount], retType);
        // Function body
        _ = blockRule(ctx, codegen);
        codegen.exitFunction(ctx);
    }
}

fn blockRule(ctx: Context, codegen: *CodeGen) BlockReturnInfo {
    _ = filterCurrentTokenOrErr(.leftBrace, ctx);
    advance(ctx);

    codegen.enterScope();
    const retInfo = blockBodyRule(ctx, codegen);

    _ = filterCurrentTokenOrErr(.rightBrace, ctx);
    codegen.exitScope();
    advance(ctx);
    return retInfo;
}

fn blockBodyRule(ctx: Context, codegen: *CodeGen) BlockReturnInfo {
    var blockRetInfo: BlockReturnInfo = .{ .returnsOnAllPaths = false };
    while (peek(ctx)) |t| {
        const subblockRetInfo: BlockReturnInfo = switch (t.tokenType) {
            .leftBrace => blockRule(ctx, codegen),
            .rightBrace => return blockRetInfo,
            else => statementRule(ctx, codegen),
        };
        blockRetInfo.returnsOnAllPaths = blockRetInfo.returnsOnAllPaths or subblockRetInfo.returnsOnAllPaths;
    }
    return blockRetInfo;
}

fn statementRule(ctx: Context, codegen: *CodeGen) BlockReturnInfo {
    const blockRetInfo = returnRule(ctx, codegen);

    const semicolonMatchOrNull = filterCurrentTokenOrErr(.semicolon, ctx);
    if (semicolonMatchOrNull != null) {
        advance(ctx);
    }
    return blockRetInfo catch .{ .returnsOnAllPaths = true };
}

fn returnRule(ctx: Context, codegen: *CodeGen) ParseInterruptSignal!BlockReturnInfo {
    const ret = try peekOrInterrupt(ctx);
    if (ret.tokenType != .kwReturn) {
        try declarationRule(ctx, codegen);
        return .{ .returnsOnAllPaths = false };
    }
    advance(ctx);
    codegen.insertFunctionReturn(ctx, try expressionRule(ctx, codegen));
    return .{ .returnsOnAllPaths = true };
}

fn declarationRule(ctx: Context, codegen: *CodeGen) ParseInterruptSignal!void {
    const iter = ctx.tokenIterator;
    const decl = try peekOrInterrupt(ctx);
    if (decl.tokenType != .kwVar) {
        _ = try expressionRule(ctx, codegen);
        return;
    }
    advance(ctx);

    const nameToken = try peekOrInterrupt(ctx);
    if (nameToken.tokenType != .identifier) {
        ctx.pushError(.{ .expectedToken = .{ .expected = .identifier, .found = nameToken } });
    }

    advance(ctx);
    switch (toi(peek(ctx)).tokenType) {
        .colon => {
            advance(ctx);

            const typeToken = peekOrInterrupt(ctx) catch |e| {
                // todo: fix
                ctx.pushError(.{ .expectedTypeToken = .{ .found = null } });
                return e;
            };
            const varType: bytecode.Type = switch (typeToken.tokenType) {
                .tyBool => .bool,
                .tyNum => .number,
                .tyString => .string,
                .tyVoid => .nil,
                else => e: {
                    ctx.pushError(.{ .expectedTypeToken = .{ .found = typeToken } });
                    break :e .nil;
                },
            };

            advance(ctx);

            const next = peek(ctx) orelse return ParseInterruptSignal.ReachedEndOfStatement;
            const initialValue: ?Handle = val: {
                switch (next.tokenType) {
                    .equal => {
                        advance(ctx);
                        break :val try expressionRule(ctx, codegen);
                    },
                    .semicolon => {
                        break :val null;
                    },
                    else => {
                        ctx.pushError(.{ .expectedToken = .{ .expected = .semicolon, .found = next } });
                        break :val null;
                    },
                }
            };

            _ = codegen.registerVariable(ctx, iter.exchangeTokenForSource(nameToken), .{ .provided = .{ .type = varType, .initial = initialValue } });
            return;
        },
        .equal => {
            advance(ctx);
            _ = codegen.registerVariable(ctx, iter.exchangeTokenForSource(nameToken), .{ .fromValue = try expressionRule(ctx, codegen) });
            return;
        },
        // Includes semicolon.
        else => {
            ctx.pushError(.expectedTypeAnnotation);
            return;
        },
    }
}

fn expressionRule(ctx: Context, codegen: *CodeGen) ParseInterruptSignal!Handle {
    if (toi(peek(ctx)).tokenType == .semicolon) {
        ctx.pushError(.expectedExpression);
        return .ERR;
    }
    return try orRule(ctx, codegen);
}

// might be the most atrocious function body i've ever written
inline fn binaryRule(ctx: Context, codegen: *CodeGen, comptime matches: []const TokenToBinaryExpr, previousRule: fn (Context, *CodeGen) ParseInterruptSignal!Handle) ParseInterruptSignal!Handle {
    var expression = try previousRule(ctx, codegen);
    while (peek(ctx)) |tok| {
        const operation = matchTokenToExprOrNull(tok.tokenType, matches) orelse break;

        advance(ctx);

        const right = try previousRule(ctx, codegen);

        expression = codegen.pushBinaryOperation(ctx, operation, expression, right);
    }
    return expression;
}
fn orRule(ctx: Context, codegen: *CodeGen) !Handle {
    const matches = &[_]TokenToBinaryExpr{.{ .key = .kwOr, .value = .bOr }};
    return binaryRule(ctx, codegen, matches, andRule);
}
fn andRule(ctx: Context, codegen: *CodeGen) !Handle {
    const matches = &[_]TokenToBinaryExpr{.{ .key = .kwAnd, .value = .bAnd }};
    return binaryRule(ctx, codegen, matches, equalityRule);
}
fn equalityRule(ctx: Context, codegen: *CodeGen) !Handle {
    const matches = &[_]TokenToBinaryExpr{ .{ .key = .bangEqual, .value = .notEquality }, .{ .key = .equalEqual, .value = .equality } };
    return binaryRule(ctx, codegen, matches, comparisonRule);
}
fn comparisonRule(ctx: Context, codegen: *CodeGen) !Handle {
    const matches = &[_]TokenToBinaryExpr{ .{ .key = .greater, .value = .greater }, .{ .key = .greaterEqual, .value = .greaterEqual }, .{ .key = .less, .value = .less }, .{ .key = .lessEqual, .value = .lessEqual } };
    return binaryRule(ctx, codegen, matches, termRule);
}
fn termRule(ctx: Context, codegen: *CodeGen) !Handle {
    const matches = &[_]TokenToBinaryExpr{ .{ .key = .plus, .value = .add }, .{ .key = .minus, .value = .subtract } };
    return binaryRule(ctx, codegen, matches, factorRule);
}
fn factorRule(ctx: Context, codegen: *CodeGen) !Handle {
    const matches = &[_]TokenToBinaryExpr{ .{ .key = .star, .value = .multiply }, .{ .key = .slash, .value = .divide }, .{ .key = .percent, .value = .modulo } };
    return binaryRule(ctx, codegen, matches, unaryRule);
}
fn unaryRule(ctx: Context, codegen: *CodeGen) ParseInterruptSignal!Handle {
    const opToken = peek(ctx);

    const operation: UnaryExprType = switch (toi(opToken).tokenType) {
        .bang => .negateBool,
        .minus => .negate,
        else => return functionCallOrVariableOrAssignmentRule(ctx, codegen),
    };

    advance(ctx);

    const right = try unaryRule(ctx, codegen);

    return codegen.pushUnaryOperation(ctx, operation, right);
}

// Calls and variable usages both start with an identifier, so they're combined into one rule.
fn functionCallOrVariableOrAssignmentRule(ctx: Context, codegen: *CodeGen) ParseInterruptSignal!Handle {
    const iter = ctx.tokenIterator;

    const name = peek(ctx) orelse return primaryRule(ctx, codegen);

    if (name.tokenType != .identifier and name.tokenType != .kwPrint) {
        return primaryRule(ctx, codegen);
    }
    advance(ctx);
    const startParen = peek(ctx) orelse {
        return Handle.NIL;
    };

    switch (startParen.tokenType) {
        .leftParen => {
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

            while (peekOrInterrupt(ctx)) |t| {
                if (t.tokenType == .rightParen) {
                    break;
                }

                const argExpr = try expressionRule(ctx, codegen);
                if (argNums < MAX_ARGS) {
                    args[argNums] = argExpr;
                } else {
                    ctx.pushError(.argLimitExceeded);
                }
                argNums += 1;

                const continuation = peekOrInterrupt(ctx) catch {
                    _ = filterCurrentTokenOrErr(.rightParen, ctx) orelse return .ERR;
                    return codegen.callFunction(ctx, iter.exchangeTokenForSource(name), args[0..argNums]);
                };
                switch (continuation.tokenType) {
                    .comma => {
                        advance(ctx);
                    },
                    .rightParen => break,
                    // If it's unrecognized, then we treat it as if it were
                    // the start of the next argument (we assume they forgot the comma).
                    else => {
                        // hacky but it works
                        _ = filterCurrentTokenOrErr(.comma, ctx);
                    },
                }
            } else |_| {}

            // Ending right parenthesis. If an interrupt occured (semicolon or EOF), do not advance
            // as the statement will want to use the current token.
            const end = peekOrInterrupt(ctx);
            if (end) |t| {
                advance(ctx);
                if (t.tokenType == .rightParen) {
                    return codegen.callFunction(ctx, iter.exchangeTokenForSource(name), args[0..argNums]);
                } else {
                    _ = filterCurrentTokenOrErr(.rightParen, ctx);
                    return .ERR;
                }
            } else |_| {
                _ = filterCurrentTokenOrErr(.rightParen, ctx);
                return .ERR;
            }
        },
        .equal => {
            advance(ctx);

            const item = try expressionRule(ctx, codegen);
            return codegen.updateVariable(ctx, iter.exchangeTokenForSource(name), item);
        },
        else => {
            return codegen.getVariable(ctx, iter.exchangeTokenForSource(name));
        },
    }
}

fn primaryRule(ctx: Context, codegen: *CodeGen) ParseInterruptSignal!Handle {
    const iter = ctx.tokenIterator;
    const tok = peekOrInterrupt(ctx) catch |e| {
        ctx.pushError(.expectedExpression);
        return e;
    };

    const result = switch (tok.tokenType) {
        .leftParen => grouping: {
            advance(ctx);
            const expr = expressionRule(ctx, codegen);

            // current will be the token following expr
            _ = filterCurrentTokenOrErr(.rightParen, ctx);
            advance(ctx);
            break :grouping expr;
        },
        .number => CodeGen.newNumberLit(std.fmt.parseFloat(f64, iter.exchangeTokenForSource(tok)) catch 0),
        .string => codegen.newStringLit(iter.exchangeTokenForSource(tok)),
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
