const scanning = @import("scanning.zig");
const std = @import("std");
const bytecode = @import("bytecode.zig");
const context = @import("errors.zig");

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

/// Token or Invalid
inline fn toi(t: ?Token) Token {
    return t orelse .{ .sourceStart = 0, .sourceEndExclusive = 0, .tokenType = .invalidChar };
}

// Tries to "filter" a token through a match. If no match, return null, and log the error.
inline fn filterCurrentTokenOrErr(tt: scanning.TokenType, ctx: *Context) ?Token {
    const log = ctx.log;
    const iter = ctx.tokenIterator;
    const token = iter.peek();
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
pub fn parseAndCompileAll(ctx: *Context, codegen: *CodeGen) !void {
    const iter = ctx.tokenIterator;
    while (iter.tryPeek()) |_| {
        try iter.functionDeclarationRule(codegen, log);
    }
    // If there is no active function, this is a no-op.
    // Otherwise (if the function has not ended by eof) this prevents a nasty bug.
    try codegen.exitFunction(log);
}

fn functionDeclarationRule(ctx: *Context, codegen: *CodeGen) !void {
    _ = self.matchCurrentOrLogErrAndNull(.kwFun, log);
    self.advance(log);

    const funNameTOrNull = self.matchCurrentOrLogErrAndNull(.identifier, log);
    self.advance(log);

    _ = self.matchCurrentOrLogErrAndNull(.leftParen, log);
    self.advance(log);

    var args: [MAX_ARGS]bytecode.ArgInfo = undefined;
    var argCount: usize = 0;

    // if EOF, main ( EOF,
    // skip parsing arguments
    const argStart: Token = self.tryPeek() orelse .{ .tokenType = .rightParen, .sourceEndExclusive = 0, .sourceStart = 0 };

    if (argStart.tokenType != .rightParen) while (self.tryPeek()) |_| {
        const arg_name = self.matchCurrentOrLogErrAndNull(.identifier, log) orelse break;
        self.advance(log);
        const typeDesignatorOrNull = self.tryPeek();
        if (typeDesignatorOrNull) |typeDesignator| {
            switch (typeDesignator.tokenType) {
                .comma => {
                    log.push(.expectedTypeAnnotation, self.iter.getCurrentTokenContext());
                },

                .colon => {
                    self.advance(log);
                    const arg_type_or_null = self.tryPeek();
                    if (arg_type_or_null) |arg_type| {
                        args[argCount] = .{
                            .type = switch (arg_type.tokenType) {
                                .tyBool => .bool,
                                .tyNum => .number,
                                .tyString => .string,
                                .tyVoid => e: {
                                    log.push(.argumentTypeCannotBeVoid, self.iter.getCurrentTokenContext());
                                    break :e .nil;
                                },
                                else => e: {
                                    log.push(.{ .expectedTypeToken = .{ .found = arg_type } }, self.iter.getCurrentTokenContext());
                                    break :e .nil;
                                },
                            },
                            .name = self.iter.exchangeTokenForSource(arg_name),
                        };
                    } else {
                        log.push(.{ .expectedTypeToken = .{ .found = null } }, self.iter.getCurrentTokenContext());
                    }
                    self.advance(log);
                },

                else => {
                    log.push(.expectedTypeAnnotation, self.iter.getCurrentTokenContext());
                    break;
                },
            }
        }

        const continuation = self.tryPeek();
        switch (toi(continuation).tokenType) {
            .rightParen => {
                if (argCount == MAX_ARGS - 1) {
                    // TODO: rework this so that we continue parsing, but not recording arguments after the limit is reached.
                    log.push(.argLimitExceeded, self.iter.getCurrentTokenContext());
                    break;
                } else {
                    argCount += 1;
                }
                break;
            },
            .comma => {
                self.advance(log);
                if (argCount == MAX_ARGS - 1) {
                    // TODO: rework this so that we continue parsing, but not recording arguments after the limit is reached.
                    log.push(.argLimitExceeded, self.iter.getCurrentTokenContext());
                    break;
                } else {
                    argCount += 1;
                }
            },
            else => log.push(.{ .expectedToken = .{ .expected = .comma, .found = continuation } }, self.iter.getCurrentTokenContext()),
        }
    };

    _ = self.matchCurrentOrLogErrAndNull(.rightParen, log);
    self.advance(log);

    const returnTypeTokenOrNull = self.tryPeek();
    const retType: bytecode.Type = ret: switch (toi(returnTypeTokenOrNull).tokenType) {
        .tyBool => {
            self.advance(log);
            break :ret .bool;
        },
        .tyNum => {
            self.advance(log);
            break :ret .number;
        },
        .tyString => {
            self.advance(log);
            break :ret .string;
        },
        .tyVoid => {
            self.advance(log);
            break :ret .nil;
        },
        // Start of function body. We assume this means void.
        //         v
        .leftBrace => break :ret .nil,
        else => {
            self.advance(log);
            log.push(.{ .expectedTypeToken = .{ .found = returnTypeTokenOrNull } }, self.iter.getCurrentTokenContext());
            break :ret .nil;
        },
    };
    if (funNameTOrNull) |funNameT| {
        const funName = self.iter.exchangeTokenForSource(funNameT);
        try codegen.enterFunction(log, funName, args[0..argCount], retType);
        // Function body
        _ = try self.blockRule(codegen, log);
        try codegen.exitFunction(log);
    }
}

fn blockRule(ctx: *Context, codegen: *CodeGen) Allocator.Error!BlockReturnInfo {
    _ = self.matchCurrentOrLogErrAndNull(.leftBrace, log);
    self.advance(log);

    codegen.enterScope();
    const retInfo = try self.blockBodyRule(codegen, log);

    _ = self.matchCurrentOrLogErrAndNull(.rightBrace, log);
    try codegen.exitScope();
    self.advance(log);
    return retInfo;
}

fn blockBodyRule(ctx: *Context, codegen: *CodeGen) !BlockReturnInfo {
    var blockRetInfo: BlockReturnInfo = .{ .returnsOnAllPaths = false };
    while (self.tryPeek()) |t| {
        const subblockRetInfo: BlockReturnInfo = switch (t.tokenType) {
            .leftBrace => try self.blockRule(codegen, log),
            .rightBrace => return blockRetInfo,
            else => try self.statementRule(codegen, log),
        };
        blockRetInfo.returnsOnAllPaths = blockRetInfo.returnsOnAllPaths or subblockRetInfo.returnsOnAllPaths;
    }
    return blockRetInfo;
}

fn statementRule(ctx: *Context, codegen: *CodeGen) !BlockReturnInfo {
    const blockRetInfo = try self.returnRule(codegen, log);

    const semicolonMatchOrNull = self.matchCurrentOrLogErrAndNull(.semicolon, log);
    if (semicolonMatchOrNull != null) {
        self.advance(log);
    }
    return blockRetInfo;
}

fn returnRule(ctx: *Context, codegen: *CodeGen) !BlockReturnInfo {
    const ret = self.tryPeek();
    if (toi(ret).tokenType != .kwReturn) {
        try self.declarationRule(codegen, log);
        return .{ .returnsOnAllPaths = false };
    }
    self.advance(log);
    try codegen.insertFunctionReturn(log, try self.expressionRule(codegen, log));
    return .{ .returnsOnAllPaths = true };
}

fn declarationRule(ctx: *Context, codegen: *CodeGen) !void {
    const decl = self.tryPeek();
    if (toi(decl).tokenType != .kwVar) {
        _ = try self.expressionRule(codegen, log);
        return;
    }
    self.advance(log);

    const nameTokenOrNull = self.matchCurrentOrLogErrAndNull(.identifier, log);

    self.advance(log);
    switch (toi(self.tryPeek()).tokenType) {
        .colon => {
            self.advance(log);

            const typeToken = self.tryPeek();
            const varType: bytecode.Type = switch (toi(typeToken).tokenType) {
                .tyBool => .bool,
                .tyNum => .number,
                .tyString => .string,
                .tyVoid => .nil,
                else => e: {
                    log.push(.{ .expectedTypeToken = .{ .found = typeToken } }, self.iter.getCurrentTokenContext());
                    break :e .nil;
                },
            };

            self.advance(log);

            const next = self.tryPeek();
            const initialValue: ?Handle = val: {
                switch (toi(next).tokenType) {
                    .semicolon => break :val null,
                    .equal => {
                        self.advance(log);
                        break :val try self.expressionRule(codegen, log);
                    },
                    else => {
                        log.push(.{ .expectedToken = .{ .expected = .semicolon, .found = next } }, self.iter.getCurrentTokenContext());
                        break :val null;
                    },
                }
            };

            if (nameTokenOrNull) |nameToken| {
                _ = try codegen.registerVariable(log, self.iter.exchangeTokenForSource(nameToken), .{ .provided = .{ .type = varType, .initial = initialValue } });
            }
            return;
        },
        .equal => {
            self.advance(log);
            if (nameTokenOrNull) |nameToken| {
                _ = try codegen.registerVariable(log, self.iter.exchangeTokenForSource(nameToken), .{ .fromValue = try self.expressionRule(codegen, log) });
            }
            return;
        },
        // Includes semicolon.
        else => {
            log.push(.expectedTypeAnnotation, self.iter.getCurrentTokenContext());
            return;
        },
    }
}

fn expressionRule(ctx: *Context, codegen: *CodeGen) Allocator.Error!Handle {
    return try self.orRule(codegen, log);
}

// might be the most atrocious function body i've ever written
fn binaryRule(ctx: *Context, codegen: *CodeGen, comptime matches: []const TokenToBinaryExpr, previousRule: fn (*AstParser, *CodeGen, *ErrorLog) Allocator.Error!Handle) !Handle {
    var expression = try previousRule(self, codegen, log);
    while (self.tryPeek()) |tok| {
        const operation = matchTokenToExprOrNull(tok.tokenType, matches) orelse break;

        self.advance(log);

        const right = try previousRule(self, codegen, log);

        expression = try codegen.pushBinaryOperation(log, operation, expression, right);
    }
    return expression;
}
fn orRule(ctx: *Context, codegen: *CodeGen) !Handle {
    const matches = &[_]TokenToBinaryExpr{.{ .key = .kwOr, .value = .bOr }};
    return self.binaryRule(log, codegen, matches, andRule);
}
fn andRule(ctx: *Context, codegen: *CodeGen) !Handle {
    const matches = &[_]TokenToBinaryExpr{.{ .key = .kwAnd, .value = .bAnd }};
    return self.binaryRule(log, codegen, matches, equalityRule);
}
fn equalityRule(ctx: *Context, codegen: *CodeGen) !Handle {
    const matches = &[_]TokenToBinaryExpr{ .{ .key = .bangEqual, .value = .notEquality }, .{ .key = .equalEqual, .value = .equality } };
    return self.binaryRule(log, codegen, matches, comparisonRule);
}
fn comparisonRule(ctx: *Context, codegen: *CodeGen) !Handle {
    const matches = &[_]TokenToBinaryExpr{ .{ .key = .greater, .value = .greater }, .{ .key = .greaterEqual, .value = .greaterEqual }, .{ .key = .less, .value = .less }, .{ .key = .lessEqual, .value = .lessEqual } };
    return self.binaryRule(log, codegen, matches, termRule);
}
fn termRule(ctx: *Context, codegen: *CodeGen) !Handle {
    const matches = &[_]TokenToBinaryExpr{ .{ .key = .plus, .value = .add }, .{ .key = .minus, .value = .subtract } };
    return self.binaryRule(log, codegen, matches, factorRule);
}
fn factorRule(ctx: *Context, codegen: *CodeGen) !Handle {
    const matches = &[_]TokenToBinaryExpr{ .{ .key = .star, .value = .multiply }, .{ .key = .slash, .value = .divide }, .{ .key = .percent, .value = .modulo } };
    return self.binaryRule(log, codegen, matches, unaryRule);
}
fn unaryRule(ctx: *Context, codegen: *CodeGen) !Handle {
    const opToken = self.tryPeek();

    const operation: UnaryExprType = switch (toi(opToken).tokenType) {
        .bang => .negateBool,
        .minus => .negate,
        else => return try self.functionCallOrVariableOrAssignmentRule(codegen, log),
    };

    self.advance(log);

    const right = try self.unaryRule(codegen, log);

    return try codegen.pushUnaryOperation(log, operation, right);
}

// Calls and variable usages both start with an identifier, so they're combined into one rule.
fn functionCallOrVariableOrAssignmentRule(ctx: *Context, codegen: *CodeGen) !Handle {
    const name = self.tryPeek() orelse return self.primaryRule(codegen, log);
    if (name.tokenType != .identifier and name.tokenType != .kwPrint) {
        return self.primaryRule(codegen, log);
    }
    self.advance(log);
    const startParen = self.tryPeek() orelse {
        return Handle.NIL;
    };

    switch (startParen.tokenType) {
        .leftParen => {
            self.advance(log);

            var args: [MAX_ARGS]Handle = undefined;
            var argNums: usize = 0;
            while (self.tryPeek()) |t| {
                if (t.tokenType == .rightParen) {
                    break;
                }

                args[argNums] = try self.expressionRule(codegen, log);
                argNums += 1;

                _ = self.matchCurrentOrLogErrAndNull(.comma, log) orelse break;

                if (argNums < MAX_ARGS) {
                    self.advance(log);
                } else {
                    log.push(.argLimitExceeded, self.iter.getCurrentTokenContext());
                }
            }

            _ = self.matchCurrentOrLogErrAndNull(.rightParen, log) orelse return .ERR;
            self.advance(log);
            return try codegen.callFunction(log, self.iter.exchangeTokenForSource(name), args[0..argNums]);
        },
        .equal => {
            self.advance(log);

            const item = try self.expressionRule(codegen, log);
            return try codegen.updateVariable(log, self.iter.exchangeTokenForSource(name), item);
        },
        else => {
            return codegen.getVariable(log, self.iter.exchangeTokenForSource(name));
        },
    }
}

fn primaryRule(ctx: *Context, codegen: *CodeGen) !Handle {
    const tok = toi(self.tryPeek());

    const result = switch (tok.tokenType) {
        .leftParen => grouping: {
            self.advance(log);
            const expr = try self.expressionRule(codegen, log);

            // current will be the token following expr
            _ = self.matchCurrentOrLogErrAndNull(.rightParen, log);
            self.advance(log);
            break :grouping expr;
        },
        .number => CodeGen.newNumberLit(std.fmt.parseFloat(f64, self.iter.exchangeTokenForSource(tok)) catch 0),
        .string => try codegen.newStringLit(self.iter.exchangeTokenForSource(tok)),
        .kwNil => CodeGen.newNilLit(),
        .kwTrue => comptime CodeGen.newBoolLit(true),
        .kwFalse => comptime CodeGen.newBoolLit(false),
        else => {
            // Since this is the last rule checked, a rejection means there's no expression.
            // If we're calling the expression rules, we definitely need one.
            log.push(.expectedExpression, self.iter.getCurrentTokenContext());
            return .ERR;
        },
    };
    self.advance(log);
    return result;
}
