const scanning = @import("scanning.zig");
const std = @import("std");
const bytecode = @import("bytecode.zig");
const common = @import("common.zig");

const MAX_ARGS = bytecode.MAX_ARGS;
const Token = scanning.Token;
const CodeGen = bytecode.BytecodeGenerator;
const Allocator = std.mem.Allocator;
const AnyWriter = std.io.AnyWriter;
const ErrorLog = common.ErrorLog;
pub const ErrorTrace = common.ErrorTrace;

const Handle = bytecode.HandledOperand;

const Error = common.Error;

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
    return t orelse .{ .source = null, .tokenType = .invalidChar };
}

pub const AstParser = struct {
    iter: *scanning.TokenIterator,
    lastToken: ?Token,

    pub fn new(iter: *scanning.TokenIterator) AstParser {
        return .{ .iter = iter, .lastToken = iter.next() };
    }

    // Tries to peek at the token at the position of our parser. Returns null if we are at the end of the list.
    inline fn tryPeek(self: *AstParser) ?Token {
        return self.lastToken;
    }

    inline fn advance(self: *AstParser) void {
        self.lastToken = self.iter.next();
    }

    inline fn matchCurrentOrLogErrAndNull(self: *AstParser, tt: scanning.TokenType, log: *ErrorLog) ?Token {
        const currentOrNull = self.tryPeek();
        if (currentOrNull) |current| {
            if (current.tokenType != tt) {
                log.push(.{ .expectedToken = .{ .expected = tt, .found = current } });
                return null;
            }
        } else {
            log.push(.{ .expectedToken = .{ .expected = tt, .found = null } });
        }
        return currentOrNull;
    }

    // The way this AST parser works is somewhat simple.
    // Each rule, described by the table above is a function.
    // The function mutates the state of the parser, moving the position forward
    // to the token immediately after the expression it returns.
    pub fn parseAndCompileAll(self: *AstParser, codegen: *CodeGen, log: *ErrorLog) !void {
        while (self.tryPeek()) |_| {
            try self.functionDeclarationRule(codegen, log);
        }
        // If there is no active function, this is a no-op.
        // Otherwise (if the function has not ended by eof) this prevents a nasty bug.
        try codegen.exitFunction(log);
    }

    fn functionDeclarationRule(self: *AstParser, codegen: *CodeGen, log: *ErrorLog) !void {
        _ = self.matchCurrentOrLogErrAndNull(.kwFun, log);
        self.advance();

        const funNameTOrNull = self.matchCurrentOrLogErrAndNull(.identifier, log);
        self.advance();

        _ = self.matchCurrentOrLogErrAndNull(.leftParen, log);
        self.advance();

        var args: [MAX_ARGS]bytecode.ArgInfo = undefined;
        var argCount: usize = 0;

        const argStart = toi(self.tryPeek());

        if (argStart.tokenType != .rightParen) while (self.tryPeek()) |_| {
            const arg_name = self.matchCurrentOrLogErrAndNull(.identifier, log) orelse break;
            self.advance();
            const typeDesignatorOrNull = self.tryPeek();
            if (typeDesignatorOrNull) |typeDesignator| {
                switch (typeDesignator.tokenType) {
                    .comma => {
                        log.push(.expectedTypeAnnotation);
                    },

                    .colon => {
                        self.advance();
                        const arg_type_or_null = self.tryPeek();
                        if (arg_type_or_null) |arg_type| {
                            args[argCount] = .{
                                .type = switch (arg_type.tokenType) {
                                    .tyBool => .bool,
                                    .tyNum => .number,
                                    .tyString => .string,
                                    .tyVoid => e: {
                                        log.push(.argumentTypeCannotBeVoid);
                                        break :e .nil;
                                    },
                                    else => e: {
                                        log.push(.{ .expectedTypeToken = .{ .found = arg_type } });
                                        break :e .nil;
                                    },
                                },
                                .name = arg_name.source orelse unreachable,
                            };
                        } else {
                            log.push(.{ .expectedTypeToken = .{ .found = null } });
                        }
                        self.advance();
                    },

                    else => {
                        log.push(.expectedTypeAnnotation);
                        break;
                    },
                }
            }

            const continuation = self.tryPeek();
            switch (toi(continuation).tokenType) {
                .rightParen => {
                    if (argCount == MAX_ARGS - 1) {
                        // TODO: rework this so that we continue parsing, but not recording arguments after the limit is reached.
                        log.push(.argLimitExceeded);
                        break;
                    } else {
                        argCount += 1;
                    }
                    break;
                },
                .comma => {
                    self.advance();
                    if (argCount == MAX_ARGS - 1) {
                        // TODO: rework this so that we continue parsing, but not recording arguments after the limit is reached.
                        log.push(.argLimitExceeded);
                        break;
                    } else {
                        argCount += 1;
                    }
                },
                else => log.push(.{ .expectedToken = .{ .expected = .comma, .found = continuation } }),
            }
        };

        _ = self.matchCurrentOrLogErrAndNull(.rightParen, log);
        self.advance();

        const returnTypeTokenOrNull = self.tryPeek();
        const retType: bytecode.Type = ret: switch (toi(returnTypeTokenOrNull).tokenType) {
            .tyBool => {
                self.advance();
                break :ret .bool;
            },
            .tyNum => {
                self.advance();
                break :ret .number;
            },
            .tyString => {
                self.advance();
                break :ret .string;
            },
            .tyVoid => {
                self.advance();
                break :ret .nil;
            },
            // Start of function body. We assume this means void.
            //         v
            .leftBrace => break :ret .nil,
            else => {
                self.advance();
                log.push(.{ .expectedTypeToken = .{ .found = returnTypeTokenOrNull } });
                break :ret .nil;
            },
        };
        if (funNameTOrNull) |funNameT| {
            const funName = funNameT.source orelse unreachable;
            try codegen.enterFunction(log, funName, args[0..argCount], retType);
            // Function body
            _ = try self.blockRule(codegen, log);
            try codegen.exitFunction(log);
        }
    }

    fn blockRule(self: *AstParser, codegen: *CodeGen, log: *ErrorLog) Allocator.Error!BlockReturnInfo {
        _ = self.matchCurrentOrLogErrAndNull(.leftBrace, log);
        self.advance();

        codegen.enterScope();
        const retInfo = try self.blockBodyRule(codegen, log);

        _ = self.matchCurrentOrLogErrAndNull(.rightBrace, log);
        try codegen.exitScope();
        self.advance();
        return retInfo;
    }

    fn blockBodyRule(self: *AstParser, codegen: *CodeGen, log: *ErrorLog) !BlockReturnInfo {
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

    fn statementRule(self: *AstParser, codegen: *CodeGen, log: *ErrorLog) !BlockReturnInfo {
        const blockRetInfo = try self.returnRule(codegen, log);

        const semicolonMatchOrNull = self.matchCurrentOrLogErrAndNull(.semicolon, log);
        if (semicolonMatchOrNull != null) {
            self.advance();
        }
        return blockRetInfo;
    }

    fn returnRule(self: *AstParser, codegen: *CodeGen, log: *ErrorLog) !BlockReturnInfo {
        const ret = self.tryPeek();
        if (toi(ret).tokenType != .kwReturn) {
            try self.declarationRule(codegen, log);
            return .{ .returnsOnAllPaths = false };
        }
        self.advance();
        try codegen.insertFunctionReturn(log, try self.expressionRule(codegen, log));
        return .{ .returnsOnAllPaths = true };
    }

    fn declarationRule(self: *AstParser, codegen: *CodeGen, log: *ErrorLog) !void {
        const decl = self.tryPeek();
        if (toi(decl).tokenType != .kwVar) {
            _ = try self.expressionRule(codegen, log);
            return;
        }
        self.advance();

        const nameTokenOrNull = self.matchCurrentOrLogErrAndNull(.identifier, log);

        self.advance();
        switch (toi(self.tryPeek()).tokenType) {
            .colon => {
                self.advance();

                const typeToken = self.tryPeek();
                const varType: bytecode.Type = switch (toi(typeToken).tokenType) {
                    .tyBool => .bool,
                    .tyNum => .number,
                    .tyString => .string,
                    .tyVoid => .nil,
                    else => e: {
                        log.push(.{ .expectedTypeToken = .{ .found = typeToken } });
                        break :e .nil;
                    },
                };

                self.advance();

                const next = self.tryPeek();
                const initialValue: ?Handle = val: {
                    switch (toi(next).tokenType) {
                        .semicolon => break :val null,
                        .equal => {
                            self.advance();
                            break :val try self.expressionRule(codegen, log);
                        },
                        else => {
                            log.push(.{ .expectedToken = .{ .expected = .semicolon, .found = next } });
                            break :val null;
                        },
                    }
                };

                if (nameTokenOrNull) |nameToken| {
                    _ = try codegen.registerVariable(log, nameToken.source orelse unreachable, .{ .provided = .{ .type = varType, .initial = initialValue } });
                }
                return;
            },
            .equal => {
                self.advance();
                if (nameTokenOrNull) |nameToken| {
                    _ = try codegen.registerVariable(log, nameToken.source orelse unreachable, .{ .fromValue = try self.expressionRule(codegen, log) });
                }
                return;
            },
            // Includes semicolon.
            else => {
                log.push(.expectedTypeAnnotation);
                return;
            },
        }
    }

    fn expressionRule(self: *AstParser, codegen: *CodeGen, log: *ErrorLog) Allocator.Error!Handle {
        return try self.orRule(codegen, log);
    }

    // might be the most atrocious function body i've ever written
    fn binaryRule(self: *AstParser, log: *ErrorLog, codegen: *CodeGen, comptime matches: []const TokenToBinaryExpr, previousRule: fn (*AstParser, *CodeGen, *ErrorLog) Allocator.Error!Handle) !Handle {
        var expression = try previousRule(self, codegen, log);
        while (self.tryPeek()) |tok| {
            const operation = matchTokenToExprOrNull(tok.tokenType, matches) orelse break;

            self.advance();

            const right = try previousRule(self, codegen, log);

            expression = try codegen.pushBinaryOperation(log, operation, expression, right);
        }
        return expression;
    }
    fn orRule(self: *AstParser, codegen: *CodeGen, log: *ErrorLog) !Handle {
        const matches = &[_]TokenToBinaryExpr{.{ .key = .kwOr, .value = .bOr }};
        return self.binaryRule(log, codegen, matches, andRule);
    }
    fn andRule(self: *AstParser, codegen: *CodeGen, log: *ErrorLog) !Handle {
        const matches = &[_]TokenToBinaryExpr{.{ .key = .kwAnd, .value = .bAnd }};
        return self.binaryRule(log, codegen, matches, equalityRule);
    }
    fn equalityRule(self: *AstParser, codegen: *CodeGen, log: *ErrorLog) !Handle {
        const matches = &[_]TokenToBinaryExpr{ .{ .key = .bangEqual, .value = .notEquality }, .{ .key = .equalEqual, .value = .equality } };
        return self.binaryRule(log, codegen, matches, comparisonRule);
    }
    fn comparisonRule(self: *AstParser, codegen: *CodeGen, log: *ErrorLog) !Handle {
        const matches = &[_]TokenToBinaryExpr{ .{ .key = .greater, .value = .greater }, .{ .key = .greaterEqual, .value = .greaterEqual }, .{ .key = .less, .value = .less }, .{ .key = .lessEqual, .value = .lessEqual } };
        return self.binaryRule(log, codegen, matches, termRule);
    }
    fn termRule(self: *AstParser, codegen: *CodeGen, log: *ErrorLog) !Handle {
        const matches = &[_]TokenToBinaryExpr{ .{ .key = .plus, .value = .add }, .{ .key = .minus, .value = .subtract } };
        return self.binaryRule(log, codegen, matches, factorRule);
    }
    fn factorRule(self: *AstParser, codegen: *CodeGen, log: *ErrorLog) !Handle {
        const matches = &[_]TokenToBinaryExpr{ .{ .key = .star, .value = .multiply }, .{ .key = .slash, .value = .divide }, .{ .key = .percent, .value = .modulo } };
        return self.binaryRule(log, codegen, matches, unaryRule);
    }
    fn unaryRule(self: *AstParser, codegen: *CodeGen, log: *ErrorLog) !Handle {
        const opToken = self.tryPeek();

        const operation: UnaryExprType = switch (toi(opToken).tokenType) {
            .bang => .negateBool,
            .minus => .negate,
            else => return try self.functionCallOrVariableOrAssignmentRule(codegen, log),
        };

        self.advance();

        const right = try self.unaryRule(codegen, log);

        return try codegen.pushUnaryOperation(log, operation, right);
    }

    // Calls and variable usages both start with an identifier, so they're combined into one rule.
    fn functionCallOrVariableOrAssignmentRule(self: *AstParser, codegen: *CodeGen, log: *ErrorLog) !Handle {
        const name = self.tryPeek() orelse return self.primaryRule(codegen, log);
        if (name.tokenType != .identifier and name.tokenType != .kwPrint) {
            return self.primaryRule(codegen, log);
        }
        self.advance();
        const startParen = self.tryPeek() orelse {
            return Handle.NIL;
        };

        switch (startParen.tokenType) {
            .leftParen => {
                self.advance();

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
                        self.advance();
                    } else {
                        log.push(.argLimitExceeded);
                    }
                }

                _ = self.matchCurrentOrLogErrAndNull(.rightParen, log) orelse return .ERR;
                self.advance();
                return try codegen.callFunction(log, name.source orelse unreachable, args[0..argNums]);
            },
            .equal => {
                self.advance();

                const item = try self.expressionRule(codegen, log);
                return try codegen.updateVariable(log, name.source orelse unreachable, item);
            },
            else => {
                return codegen.getVariable(log, name.source orelse unreachable);
            },
        }
    }

    fn primaryRule(self: *AstParser, codegen: *CodeGen, log: *ErrorLog) !Handle {
        const tok = toi(self.tryPeek());

        const result = switch (tok.tokenType) {
            .leftParen => grouping: {
                self.advance();
                const expr = try self.expressionRule(codegen, log);

                // current will be the token following expr
                _ = self.matchCurrentOrLogErrAndNull(.rightParen, log);
                self.advance();
                break :grouping expr;
            },
            .number => CodeGen.newNumberLit(std.fmt.parseFloat(f64, tok.source orelse unreachable) catch 0),
            .string => try codegen.newStringLit(tok.source orelse unreachable),
            .kwNil => CodeGen.newNilLit(),
            .kwTrue => comptime CodeGen.newBoolLit(true),
            .kwFalse => comptime CodeGen.newBoolLit(false),
            else => {
                // Since this is the last rule checked, a rejection means there's no expression.
                // If we're calling the expression rules, we definitely need one.
                log.push(.expectedExpression);
                return .ERR;
            },
        };
        self.advance();
        return result;
    }
};
