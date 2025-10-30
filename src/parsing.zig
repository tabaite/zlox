const scanning = @import("scanning.zig");
const std = @import("std");
const bytecode = @import("bytecode.zig");
const common = @import("common.zig");

// program        → ( statement )* EOF
// function       → "fun" IDENTIFIER "(" ( (IDENTIFIER ":" type ",")* (IDENTIFIER ":" type) )? ")" ( type )? block
// arg            → IDENTIFIER ":" type; malformed: IDENTIFIER
// block          → "{" ( statement )* "}"
// statement      → ( return | decl | expression ) ";"
// return         → "return" expression
// declaration    → "var" IDENTIFIER ( ":" type )? ( "=" expression )?
// assignment     → IDENTIFIER "=" expression
// expression     → or
// or             → and "or" and
// and            → equality "and" equality
// equality       → comparison ( ( "!=" | "==" ) comparison )*
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )*
// term           → factor ( ( "-" | "+" ) factor )*
// factor         → unary ( ( "/" | "*" | "%" ) unary )*
// unary          → ( "!" | "-" ) unary
//                | call | primary
// call           → IDENTIFIER "(" ( ( expression "," )* expression ) ")"
// primary        → NUMBER | STRING | "true" | "false" | "nil"
//                | "(" expression ")"
// type           → "number" | "bool" | "string" | "void"

const MAX_ARGS = bytecode.MAX_ARGS;
const Token = scanning.Token;
const CodeGen = bytecode.BytecodeGenerator;
const Allocator = std.mem.Allocator;
const AnyWriter = std.io.AnyWriter;
const ErrorLog = common.ErrorLog;
pub const ErrorTrace = common.ErrorTrace;

const Handle = bytecode.HandledOperand;

const ErrorSet = ParsingError || bytecode.CompilationError;
const ParseErrorSet = Allocator.Error || ErrorSet;

pub const ParsingError = error{
    GlobalScopeNoLongerUsable,
    UnexpectedToken,
    ExpectedToken,
    ExpectedKwFun,
    ExpectedComma,
    ExpectedSemicolon,
    ArgLimit128,
    ExpectedOpeningParen,
    ExpectedClosingParen,
    ExpectedOpeningBrace,
    ExpectedClosingBrace,
    ExpectedIdentifier,
    ExpectedExpression,
    ExpectedType,
    CustomTypesNotYetSupported,
    ArgumentCannotBeTypeVoid,
    ExpectedTypeAtVariableDeclaration,
};

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

const FunctionContext = struct {
    retType: bytecode.Type,
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

fn matchTokenToExprOrNull(target: scanning.TokenType, matches: []const TokenToBinaryExpr) ?BinaryExprType {
    for (matches) |t| {
        if (t.key == target) {
            return t.value;
        }
    }
    return null;
}

pub const AstParser = struct {
    iter: *scanning.TokenIterator,
    lastToken: ?Token,
    errorList: std.ArrayList(ErrorTrace),

    pub fn new(iter: *scanning.TokenIterator, allocator: Allocator) AstParser {
        return .{ .iter = iter, .lastToken = iter.next(), .errorList = .init(allocator) };
    }

    // Tries to peek at the token at the position of our parser. Returns null if we are at the end of the list.
    pub fn tryPeek(self: *AstParser) ?Token {
        return self.lastToken;
    }

    fn advance(self: *AstParser) void {
        self.lastToken = self.iter.next();
    }

    // The way this AST parser works is somewhat simple.
    // Each rule, described by the table above is a function.
    // The function mutates the state of the parser, moving the position forward
    // to the token immediately after the expression it returns.
    pub fn parseAndCompileAll(self: *AstParser, codegen: *CodeGen, log: *ErrorLog) !void {
        while (self.tryPeek()) |t| {
            switch (t.tokenType) {
                .leftBrace => _ = try self.blockRule(codegen, log),
                .kwFun => try self.functionDeclarationRule(codegen, log),
                else => {
                    self.recordErrorTrace(log, ParsingError.GlobalScopeNoLongerUsable);
                    _ = try self.statementRule(codegen, log);
                },
            }
        }
        // If there is no active function, this is a no-op.
        // Otherwise (if the function has not ended by eof) this prevents a nasty bug.
        try codegen.exitFunction();
    }

    fn recordErrorTrace(_: *AstParser, log: *ErrorLog, err: ParsingError) void {
        log.push(err);
    }

    fn functionDeclarationRule(self: *AstParser, codegen: *CodeGen, log: *ErrorLog) !void {
        const fun: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
        if (fun.tokenType != .kwFun) {
            self.recordErrorTrace(log, ParsingError.ExpectedKwFun);
        }
        self.advance();
        const funNameT: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
        if (funNameT.tokenType != .identifier) {
            self.recordErrorTrace(log, ParsingError.ExpectedIdentifier);
        }
        self.advance();
        const open: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
        if (open.tokenType != .leftParen) {
            self.recordErrorTrace(log, ParsingError.ExpectedOpeningParen);
        }
        self.advance();

        var args: [MAX_ARGS]bytecode.ArgInfo = undefined;
        var argCount: usize = 0;

        const argStart = self.tryPeek() orelse Token{ .source = null, .tokenType = .invalidChar };
        if (argStart.tokenType != .rightParen) args: while (self.tryPeek()) |_| {
            const argNameT: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
            if (argNameT.tokenType != .identifier) {
                self.recordErrorTrace(log, ParsingError.ExpectedIdentifier);
                args[argCount] = .{ .type = .nil, .name = @constCast("a") };
                break :args;
            } else argType: {
                self.advance();
                const colon: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
                if (colon.tokenType != .colon) {
                    self.recordErrorTrace(log, ParsingError.ExpectedType);
                    break :argType;
                }
                self.advance();
                const argType: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
                args[argCount] = .{
                    .type = switch (argType.tokenType) {
                        .tyBool => .bool,
                        .tyNum => .number,
                        .tyString => .string,
                        .tyVoid => e: {
                            self.recordErrorTrace(log, ParsingError.ArgumentCannotBeTypeVoid);
                            break :e .nil;
                        },
                        else => e: {
                            self.recordErrorTrace(log, ParsingError.ExpectedType);
                            break :e .nil;
                        },
                    },
                    .name = argNameT.source orelse unreachable,
                };
            }
            self.advance();
            const continuation: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
            switch (continuation.tokenType) {
                .rightParen => {
                    if (argCount == MAX_ARGS - 1) {
                        // TODO: rework this so that we continue parsing, but not recording arguments after the limit is reached.
                        self.recordErrorTrace(log, ParsingError.ArgLimit128);
                        break :args;
                    } else {
                        argCount += 1;
                    }
                    break :args;
                },
                .comma => {
                    self.advance();
                    if (argCount == MAX_ARGS - 1) {
                        // TODO: rework this so that we continue parsing, but not recording arguments after the limit is reached.
                        self.recordErrorTrace(log, ParsingError.ArgLimit128);
                        break :args;
                    } else {
                        argCount += 1;
                    }
                },
                else => self.recordErrorTrace(log, ParsingError.ExpectedComma),
            }
        };

        const close: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
        if (close.tokenType != .rightParen) {
            self.recordErrorTrace(log, ParsingError.ExpectedClosingParen);
        }
        self.advance();
        const rt: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
        const retType: bytecode.Type = ret: switch (rt.tokenType) {
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
            // Start of function body. We assume this means void.
            //         v
            .tyVoid => {
                self.advance();
                break :ret .nil;
            },
            .leftBrace => break :ret .nil,
            else => {
                self.advance();
                self.recordErrorTrace(log, ParsingError.ExpectedType);
                break :ret .nil;
            },
        };
        const funName = funNameT.source orelse unreachable;
        try codegen.enterFunction(log, funName, args[0..argCount], retType);
        // Function body
        _ = try self.blockRule(codegen, log);
        try codegen.exitFunction();
    }

    fn blockRule(self: *AstParser, codegen: *CodeGen, log: *ErrorLog) ParseErrorSet!BlockReturnInfo {
        const opening: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
        if (opening.tokenType != .leftBrace) {
            self.recordErrorTrace(log, ParsingError.ExpectedOpeningBrace);
        }
        self.advance();
        codegen.enterScope();
        const retInfo = try self.blockBodyRule(codegen, log);
        const closing: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
        if (closing.tokenType != .rightBrace) {
            self.recordErrorTrace(log, ParsingError.ExpectedClosingBrace);
        }
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
        const end: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
        if (end.tokenType != .semicolon) {
            self.recordErrorTrace(log, ParsingError.ExpectedSemicolon);
        } else {
            self.advance();
        }
        return blockRetInfo;
    }

    fn returnRule(self: *AstParser, codegen: *CodeGen, log: *ErrorLog) !BlockReturnInfo {
        const ret: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
        if (ret.tokenType != .kwReturn) {
            try self.declarationRule(codegen, log);
            return .{ .returnsOnAllPaths = false };
        }
        self.advance();
        try codegen.insertFunctionReturn(try self.expressionRule(codegen, log));
        return .{ .returnsOnAllPaths = true };
    }

    fn declarationRule(self: *AstParser, codegen: *CodeGen, log: *ErrorLog) !void {
        const decl: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
        if (decl.tokenType != .kwVar) {
            _ = try self.expressionRule(codegen, log);
            return;
        }
        self.advance();
        const name: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
        if (name.tokenType != .identifier) {
            self.recordErrorTrace(log, ParsingError.ExpectedIdentifier);
        }

        self.advance();
        switch ((self.tryPeek() orelse Token{ .source = null, .tokenType = .invalidChar }).tokenType) {
            .colon => {
                self.advance();

                const typeToken: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
                const varType: bytecode.Type = switch (typeToken.tokenType) {
                    .tyBool => .bool,
                    .tyNum => .number,
                    .tyString => .string,
                    .tyVoid => .nil,
                    .identifier => e: {
                        self.recordErrorTrace(log, ParsingError.CustomTypesNotYetSupported);
                        break :e .nil;
                    },
                    else => e: {
                        self.recordErrorTrace(log, ParsingError.ExpectedType);
                        break :e .nil;
                    },
                };

                self.advance();

                const next: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
                const initialValue: ?Handle = val: {
                    switch (next.tokenType) {
                        .semicolon => break :val null,
                        .equal => {
                            self.advance();
                            break :val try self.expressionRule(codegen, log);
                        },
                        else => {
                            self.recordErrorTrace(log, ParsingError.ExpectedToken);
                            break :val null;
                        },
                    }
                };
                _ = try codegen.registerVariable(log, name.source orelse @constCast("NULLSFEPIRUPWUREWIP"), .{ .provided = .{ .type = varType, .initial = initialValue } });
                return;
            },
            .semicolon => {
                self.recordErrorTrace(log, ParsingError.ExpectedTypeAtVariableDeclaration);
                return;
            },
            .equal => {
                self.advance();
                _ = try codegen.registerVariable(log, name.source orelse @constCast("NULLSFEPIRUPWUREWIP"), .{ .fromValue = try self.expressionRule(codegen, log) });
                return;
            },
            else => {
                self.recordErrorTrace(log, ParsingError.ExpectedToken);
                return;
            },
        }
    }

    fn expressionRule(self: *AstParser, codegen: *CodeGen, log: *ErrorLog) !Handle {
        return try self.orRule(codegen, log);
    }

    // might be the most atrocious function body i've ever written
    fn binaryRule(self: *AstParser, log: *ErrorLog, codegen: *CodeGen, matches: []const TokenToBinaryExpr, previousRule: fn (*AstParser, *CodeGen, *ErrorLog) ParseErrorSet!Handle) ParseErrorSet!Handle {
        var expression = try previousRule(self, codegen, log);
        while (self.tryPeek()) |token| {
            const operation = matchTokenToExprOrNull(token.tokenType, matches) orelse break;

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
        const opToken: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };

        const operation: UnaryExprType = switch (opToken.tokenType) {
            .bang => .negateBool,
            .minus => .negate,
            else => return try self.functionCallOrVariableOrAssignmentRule(codegen, log),
        };

        self.advance();

        const right = try self.unaryRule(codegen, log);

        return try codegen.pushUnaryOperation(log, operation, right);
    }

    // Calls and variable usages both start with an identifier, so they're combined into one rule.
    fn functionCallOrVariableOrAssignmentRule(self: *AstParser, codegen: *CodeGen, log: *ErrorLog) ParseErrorSet!Handle {
        const name: Token = self.tryPeek() orelse return self.primaryRule(codegen, log);
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

                    const seperator: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
                    if (seperator.tokenType != .comma) {
                        break;
                    }
                    if (argNums < MAX_ARGS) {
                        self.advance();
                    } else {
                        self.recordErrorTrace(log, ParsingError.ArgLimit128);
                    }
                }

                const endParen: Token = self.tryPeek() orelse {
                    log.push(ParsingError.ExpectedClosingBrace);
                    return .ERR;
                };
                if (endParen.tokenType != .rightParen) {
                    log.push(ParsingError.ExpectedClosingBrace);
                    return .ERR;
                }
                self.advance();
                try codegen.callFunction(log, name.source orelse unreachable, args[0..argNums]);
                return Handle.NIL;
            },
            .equal => {
                self.advance();

                const item = try self.expressionRule(codegen, log);
                return try codegen.updateVariable(log, name.source orelse @constCast("NULL NAME THIS IS A BUG PLEASE REPORT IT NOW!!"), item);
            },
            else => {
                return codegen.getVariable(log, name.source orelse @constCast("uhhhhhhhhh this is awkward"));
            },
        }
    }

    fn primaryRule(self: *AstParser, codegen: *CodeGen, log: *ErrorLog) ParseErrorSet!Handle {
        const token: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };

        const result = switch (token.tokenType) {
            .leftParen => grouping: {
                const expr = try self.expressionRule(codegen, log);

                // the token will be the token following expr
                const current: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
                if (current.tokenType != .rightParen) {
                    self.recordErrorTrace(log, ParsingError.ExpectedClosingParen);
                }
                self.advance();
                break :grouping expr;
            },
            .number => CodeGen.newNumberLit(std.fmt.parseFloat(f64, token.source orelse unreachable) catch 0),
            .string => try codegen.newStringLit(token.source orelse unreachable),
            .kwNil => CodeGen.newNilLit(),
            .kwTrue => comptime CodeGen.newBoolLit(true),
            .kwFalse => comptime CodeGen.newBoolLit(false),
            else => {
                // Since this is the last rule checked, a rejection means there's no expression.
                // If we're calling the expression rules, we definitely need one.
                log.push(ParsingError.ExpectedExpression);
                return .ERR;
            },
        };
        self.advance();
        return result;
    }
};
