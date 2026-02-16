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

// HELPERS
inline fn matchTokenToExprOrNull(target: scanning.TokenType, comptime matches: []const TokenToBinaryExpr) ?BinaryExprType {
    inline for (matches) |t| {
        if (t.key == target) {
            return t.value;
        }
    }
    return null;
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
pub fn parseAndCompileAll(ctx: Context, codegen: *CodeGen) !void {
    const iter = ctx.tokenIterator;
    const log = ctx.log;
    while (iter.peek(log)) |_| {
        try functionDeclarationRule(ctx, codegen);
    }
    // If there is no active function, this is a no-op.
    // Otherwise (if the function has not ended by eof) this prevents a nasty bug.
    try codegen.exitFunction(log);
}

fn functionDeclarationRule(ctx: Context, codegen: *CodeGen) !void {
    const iter = ctx.tokenIterator;
    const log = ctx.log;
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
        try codegen.enterFunction(log, funName, args[0..argCount], retType);
        // Function body
        _ = try blockRule(codegen, log);
        try codegen.exitFunction(log);
    }
}

fn blockRule(ctx: Context, codegen: *CodeGen) Allocator.Error!BlockReturnInfo {
    _ = filterCurrentTokenOrErr(.leftBrace, ctx);
    advance(ctx);

    codegen.enterScope();
    const retInfo = try blockBodyRule(ctx, codegen);

    _ = filterCurrentTokenOrErr(.rightBrace, ctx);
    try codegen.exitScope();
    advance(ctx);
    return retInfo;
}

fn blockBodyRule(ctx: Context, codegen: *CodeGen) !BlockReturnInfo {
    var blockRetInfo: BlockReturnInfo = .{ .returnsOnAllPaths = false };
    while (peek(ctx)) |t| {
        const subblockRetInfo: BlockReturnInfo = switch (t.tokenType) {
            .leftBrace => try blockRule(ctx, codegen),
            .rightBrace => return blockRetInfo,
            else => try statementRule(ctx, codegen),
        };
        blockRetInfo.returnsOnAllPaths = blockRetInfo.returnsOnAllPaths or subblockRetInfo.returnsOnAllPaths;
    }
    return blockRetInfo;
}

fn statementRule(ctx: Context, codegen: *CodeGen) !BlockReturnInfo {
    const blockRetInfo = try returnRule(ctx, codegen);

    const semicolonMatchOrNull = filterCurrentTokenOrErr(.semicolon, ctx);
    if (semicolonMatchOrNull != null) {
        advance(ctx);
    }
    return blockRetInfo;
}

fn returnRule(ctx: Context, codegen: *CodeGen) !BlockReturnInfo {
    const log = ctx.log;
    const ret = peek(ctx);
    if (toi(ret).tokenType != .kwReturn) {
        try declarationRule(ctx, codegen);
        return .{ .returnsOnAllPaths = false };
    }
    advance(ctx);
    try codegen.insertFunctionReturn(log, try expressionRule(ctx, codegen));
    return .{ .returnsOnAllPaths = true };
}

fn declarationRule(ctx: Context, codegen: *CodeGen) !void {
    const iter = ctx.tokenIterator;
    const log = ctx.log;
    const decl = peek(ctx);
    if (toi(decl).tokenType != .kwVar) {
        _ = try expressionRule(ctx, codegen);
        return;
    }
    advance(ctx);

    const nameTokenOrNull = filterCurrentTokenOrErr(.identifier, ctx);

    advance(ctx);
    switch (toi(peek(ctx)).tokenType) {
        .colon => {
            advance(ctx);

            const typeToken = peek(ctx);
            const varType: bytecode.Type = switch (toi(typeToken).tokenType) {
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

            const next = peek(ctx);
            const initialValue: ?Handle = val: {
                switch (toi(next).tokenType) {
                    .semicolon => break :val null,
                    .equal => {
                        advance(ctx);
                        break :val try expressionRule(ctx, codegen);
                    },
                    else => {
                        ctx.pushError(.{ .expectedToken = .{ .expected = .semicolon, .found = next } });
                        break :val null;
                    },
                }
            };

            if (nameTokenOrNull) |nameToken| {
                _ = try codegen.registerVariable(log, iter.exchangeTokenForSource(nameToken), .{ .provided = .{ .type = varType, .initial = initialValue } });
            }
            return;
        },
        .equal => {
            advance(ctx);
            if (nameTokenOrNull) |nameToken| {
                _ = try codegen.registerVariable(log, iter.exchangeTokenForSource(nameToken), .{ .fromValue = try expressionRule(ctx, codegen) });
            }
            return;
        },
        // Includes semicolon.
        else => {
            ctx.pushError(.expectedTypeAnnotation);
            return;
        },
    }
}

fn expressionRule(ctx: Context, codegen: *CodeGen) Allocator.Error!Handle {
    return try orRule(ctx, codegen);
}

// might be the most atrocious function body i've ever written
fn binaryRule(ctx: Context, codegen: *CodeGen, comptime matches: []const TokenToBinaryExpr, previousRule: fn (Context, *CodeGen) Allocator.Error!Handle) !Handle {
    const log = ctx.log;
    var expression = try previousRule(ctx, codegen);
    while (peek(ctx)) |tok| {
        const operation = matchTokenToExprOrNull(tok.tokenType, matches) orelse break;

        advance(ctx);

        const right = try previousRule(ctx, codegen);

        expression = try codegen.pushBinaryOperation(log, operation, expression, right);
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
fn unaryRule(ctx: Context, codegen: *CodeGen) !Handle {
    const opToken = peek(ctx);

    const operation: UnaryExprType = switch (toi(opToken).tokenType) {
        .bang => .negateBool,
        .minus => .negate,
        else => return try functionCallOrVariableOrAssignmentRule(ctx, codegen),
    };

    advance(ctx);

    const right = try unaryRule(ctx, codegen);

    return try codegen.pushUnaryOperation(ctx.log, operation, right);
}

// Calls and variable usages both start with an identifier, so they're combined into one rule.
fn functionCallOrVariableOrAssignmentRule(ctx: Context, codegen: *CodeGen) !Handle {
    const iter = ctx.tokenIterator;
    const log = ctx.log;
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
            advance(ctx);

            var args: [MAX_ARGS]Handle = undefined;
            var argNums: usize = 0;
            while (peek(ctx)) |t| {
                if (t.tokenType == .rightParen) {
                    break;
                }

                args[argNums] = try expressionRule(ctx, codegen);
                argNums += 1;

                _ = filterCurrentTokenOrErr(.comma, ctx) orelse break;

                if (argNums < MAX_ARGS) {
                    advance(ctx);
                } else {
                    ctx.pushError(.argLimitExceeded);
                }
            }

            _ = filterCurrentTokenOrErr(.rightParen, ctx) orelse return .ERR;
            advance(ctx);
            return try codegen.callFunction(log, iter.exchangeTokenForSource(name), args[0..argNums]);
        },
        .equal => {
            advance(ctx);

            const item = try expressionRule(ctx, codegen);
            return try codegen.updateVariable(log, iter.exchangeTokenForSource(name), item);
        },
        else => {
            return codegen.getVariable(log, iter.exchangeTokenForSource(name));
        },
    }
}

fn primaryRule(ctx: Context, codegen: *CodeGen) !Handle {
    const iter = ctx.tokenIterator;
    const tok = toi(peek(ctx));

    const result = switch (tok.tokenType) {
        .leftParen => grouping: {
            advance(ctx);
            const expr = try expressionRule(ctx, codegen);

            // current will be the token following expr
            _ = filterCurrentTokenOrErr(.rightParen, ctx);
            advance(ctx);
            break :grouping expr;
        },
        .number => CodeGen.newNumberLit(std.fmt.parseFloat(f64, iter.exchangeTokenForSource(tok)) catch 0),
        .string => try codegen.newStringLit(iter.exchangeTokenForSource(tok)),
        .kwNil => CodeGen.newNilLit(),
        .kwTrue => comptime CodeGen.newBoolLit(true),
        .kwFalse => comptime CodeGen.newBoolLit(false),
        else => {
            // Since this is the last rule checked, a rejection means there's no expression.
            // If we're calling the expression rules, we definitely need one.
            ctx.pushError(.expectedExpression);
            return .ERR;
        },
    };
    advance(ctx);
    return result;
}
