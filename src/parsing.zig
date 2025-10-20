const scanning = @import("scanning.zig");
const std = @import("std");
const bytecode = @import("bytecode.zig");

// program        → ( statement )* EOF
// function       → "fun" IDENTIFIER "(" ( (IDENTIFIER ":" type ",")* (IDENTIFIER ":" type) )? ")" ( type )? block
// arg            → IDENTIFIER ":" type; malformed: IDENTIFIER
// block          → "{" ( statement )* "}"
// statement      → ( decl | expression ) ";"
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

const Token = scanning.Token;
const CodeGen = bytecode.BytecodeGenerator;
const Allocator = std.mem.Allocator;
const AnyWriter = std.io.AnyWriter;

const Handle = bytecode.HandledOperand;

const ErrorSet = ParsingError || bytecode.CompilationError;
const ParseErrorSet = Allocator.Error || ErrorSet;

pub const ParsingError = error{
    UnexpectedToken,
    ExpectedToken,
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
    VariableDeclarationMustBeTyped,
};

pub const ErrorTrace = struct {
    err: ErrorSet,
    lineNum: u32,
    line: []u8,
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

    pub fn recoverErrorList(self: *AstParser) []ErrorTrace {
        return self.errorList.items;
    }

    pub fn parseAndCompileAll(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !void {
        while (self.tryPeek()) |t| {
            switch (t.tokenType) {
                .leftBrace => try self.blockRule(codegen, allocator),
                .kwFun => try self.functionDeclarationRule(codegen, allocator),
                else => try self.statementRule(codegen, allocator),
            }
        }
    }

    fn recordErrorTrace(self: *AstParser, err: ParsingError) !void {
        var start: usize = 0;
        var end = self.iter.source.len;

        const pos = self.iter.position;
        for (0..pos) |i| {
            const idx = pos - i - 1;
            if (self.iter.source[idx] == '\n') {
                start = idx;
                break;
            }
        }
        for (pos..self.iter.source.len) |i| {
            if (self.iter.source[i] == '\n') {
                end = i;
                break;
            }
        }
        const trace: ErrorTrace = .{ .err = err, .lineNum = self.iter.lineNumber, .line = self.iter.source[start..end] };
        try self.errorList.append(trace);
    }

    fn functionDeclarationRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !void {
        const fun: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
        if (fun.tokenType != .kwFun) {
            try self.recordErrorTrace(ParsingError.ExpectedToken);
        }
        self.advance();
        const funNameT: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
        if (funNameT.tokenType != .identifier) {
            try self.recordErrorTrace(ParsingError.ExpectedIdentifier);
        }
        self.advance();
        const open: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
        if (open.tokenType != .leftParen) {
            try self.recordErrorTrace(ParsingError.ExpectedOpeningParen);
        }
        self.advance();

        var args: [128]bytecode.Type = undefined;
        var argCount: usize = 0;

        args: while (self.tryPeek()) |t| {
            if (t.tokenType == .rightParen) {
                break :args;
            }
            const argNameT: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
            if (argNameT.tokenType != .identifier) {
                try self.recordErrorTrace(ParsingError.ExpectedIdentifier);
                args[argCount] = .nil;
                break :args;
            } else argType: {
                self.advance();
                const colon: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
                if (colon.tokenType != .colon) {
                    try self.recordErrorTrace(ParsingError.ExpectedType);
                    break :argType;
                }
                self.advance();
                const argType: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
                args[argCount] = switch (argType.tokenType) {
                    .tyBool => .bool,
                    .tyNum => .number,
                    .tyString => .string,
                    .tyVoid => e: {
                        try self.recordErrorTrace(ParsingError.ArgumentCannotBeTypeVoid);
                        break :e .nil;
                    },
                    else => e: {
                        try self.recordErrorTrace(ParsingError.ExpectedType);
                        break :e .nil;
                    },
                };
            }
            self.advance();
            const comma: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
            if (comma.tokenType != .comma) {
                try self.recordErrorTrace(ParsingError.ExpectedToken);
            }
            self.advance();
            if (argCount == 127) {
                // TODO: rework this so that we continue parsing, but not recording arguments after the limit is reached.
                try self.recordErrorTrace(ParsingError.ArgLimit128);
                break :args;
            } else {
                argCount += 1;
            }
        }

        const close: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
        if (close.tokenType != .rightParen) {
            try self.recordErrorTrace(ParsingError.ExpectedClosingParen);
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
                try self.recordErrorTrace(ParsingError.ExpectedType);
                break :ret .nil;
            },
        };
        const funName = funNameT.source orelse unreachable;
        try codegen.enterFunction(funName, args[0..argCount], retType);
        // Function body
        _ = try self.blockRule(codegen, allocator);
        codegen.exitFunction();
    }

    fn blockRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) ParseErrorSet!void {
        const opening: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
        if (opening.tokenType != .leftBrace) {
            try self.recordErrorTrace(ParsingError.ExpectedOpeningBrace);
        }
        self.advance();
        codegen.enterScope();
        try self.blockBodyRule(codegen, allocator);
        const closing: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
        if (closing.tokenType != .rightBrace) {
            try self.recordErrorTrace(ParsingError.ExpectedClosingBrace);
        }
        try codegen.exitScope();
        self.advance();
    }

    fn blockBodyRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !void {
        while (self.tryPeek()) |t| {
            switch (t.tokenType) {
                .leftBrace => try self.blockRule(codegen, allocator),
                .rightBrace => return,
                else => try self.statementRule(codegen, allocator),
            }
        }
    }

    fn statementRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !void {
        _ = try self.declarationRule(codegen, allocator);
        const end: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
        if (end.tokenType != .semicolon) {
            try self.recordErrorTrace(ParsingError.ExpectedSemicolon);
        } else {
            self.advance();
        }
    }

    fn declarationRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !Handle {
        const decl: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
        if (decl.tokenType != .kwVar) {
            return self.expressionRule(codegen, allocator);
        }
        self.advance();
        const name: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
        if (name.tokenType != .identifier) {
            try self.recordErrorTrace(ParsingError.ExpectedIdentifier);
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
                        try self.recordErrorTrace(ParsingError.CustomTypesNotYetSupported);
                        break :e .nil;
                    },
                    else => e: {
                        try self.recordErrorTrace(ParsingError.ExpectedType);
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
                            break :val try self.expressionRule(codegen, allocator);
                        },
                        else => {
                            try self.recordErrorTrace(ParsingError.ExpectedToken);
                            break :val null;
                        },
                    }
                };
                return try codegen.registerVariable(name.source orelse @constCast("NULLSFEPIRUPWUREWIP"), .{ .provided = .{ .type = varType, .initial = initialValue } });
            },
            .semicolon => {
                try self.recordErrorTrace(ParsingError.VariableDeclarationMustBeTyped);
                return .NIL;
            },
            .equal => {
                self.advance();
                return try codegen.registerVariable(name.source orelse @constCast("NULLSFEPIRUPWUREWIP"), .{ .fromValue = try self.expressionRule(codegen, allocator) });
            },
            else => {
                try self.recordErrorTrace(ParsingError.ExpectedToken);
                return .NIL;
            },
        }
    }

    fn expressionRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !Handle {
        return try self.orRule(codegen, allocator);
    }

    // might be the most atrocious function body i've ever written
    fn binaryRule(self: *AstParser, allocator: Allocator, codegen: *CodeGen, matches: []const TokenToBinaryExpr, previousRule: fn (*AstParser, *CodeGen, std.mem.Allocator) ParseErrorSet!Handle) ParseErrorSet!Handle {
        var expression = try previousRule(self, codegen, allocator);
        while (self.tryPeek()) |token| {
            const operation = matchTokenToExprOrNull(token.tokenType, matches) orelse break;

            self.advance();

            const right = try previousRule(self, codegen, allocator);

            expression = try codegen.pushBinaryOperation(operation, expression, right);
        }
        return expression;
    }
    fn orRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !Handle {
        const matches = &[_]TokenToBinaryExpr{.{ .key = .kwOr, .value = .bOr }};
        return self.binaryRule(allocator, codegen, matches, andRule);
    }
    fn andRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !Handle {
        const matches = &[_]TokenToBinaryExpr{.{ .key = .kwAnd, .value = .bAnd }};
        return self.binaryRule(allocator, codegen, matches, equalityRule);
    }
    fn equalityRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !Handle {
        const matches = &[_]TokenToBinaryExpr{ .{ .key = .bangEqual, .value = .notEquality }, .{ .key = .equalEqual, .value = .equality } };
        return self.binaryRule(allocator, codegen, matches, comparisonRule);
    }
    fn comparisonRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !Handle {
        const matches = &[_]TokenToBinaryExpr{ .{ .key = .greater, .value = .greater }, .{ .key = .greaterEqual, .value = .greaterEqual }, .{ .key = .less, .value = .less }, .{ .key = .lessEqual, .value = .lessEqual } };
        return self.binaryRule(allocator, codegen, matches, termRule);
    }
    fn termRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !Handle {
        const matches = &[_]TokenToBinaryExpr{ .{ .key = .plus, .value = .add }, .{ .key = .minus, .value = .subtract } };
        return self.binaryRule(allocator, codegen, matches, factorRule);
    }
    fn factorRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !Handle {
        const matches = &[_]TokenToBinaryExpr{ .{ .key = .star, .value = .multiply }, .{ .key = .slash, .value = .divide }, .{ .key = .percent, .value = .modulo } };
        return self.binaryRule(allocator, codegen, matches, unaryRule);
    }
    fn unaryRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !Handle {
        const opToken: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };

        const operation: UnaryExprType = switch (opToken.tokenType) {
            .bang => .negateBool,
            .minus => .negate,
            else => return try self.functionCallOrVariableOrAssignmentRule(codegen, allocator),
        };

        self.advance();

        const right = try self.unaryRule(codegen, allocator);

        return try codegen.pushUnaryOperation(operation, right);
    }

    // Calls and variable usages both start with an identifier, so they're combined into one rule.
    fn functionCallOrVariableOrAssignmentRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) ParseErrorSet!Handle {
        const name: Token = self.tryPeek() orelse return self.primaryRule(codegen, allocator);
        if (name.tokenType != .identifier and name.tokenType != .kwPrint) {
            return self.primaryRule(codegen, allocator);
        }
        self.advance();
        const startParen = self.tryPeek() orelse {
            return Handle.NIL;
        };

        switch (startParen.tokenType) {
            .leftParen => {
                self.advance();

                var args: [128]Handle = undefined;
                var argNums: usize = 0;
                while (self.tryPeek()) |t| {
                    if (t.tokenType == .rightParen) {
                        break;
                    }

                    args[argNums] = try self.expressionRule(codegen, allocator);
                    argNums += 1;

                    const seperator: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
                    if (seperator.tokenType != .comma) {
                        break;
                    }
                    if (argNums < 128) {
                        self.advance();
                    } else {
                        try self.recordErrorTrace(ParsingError.ArgLimit128);
                    }
                }

                const endParen: Token = self.tryPeek() orelse return ParsingError.ExpectedClosingBrace;
                if (endParen.tokenType != .rightParen) {
                    return ParsingError.ExpectedClosingBrace;
                }
                self.advance();
                return Handle.NIL;
            },
            .equal => {
                self.advance();

                const item = try self.expressionRule(codegen, allocator);
                return codegen.updateVariable(name.source orelse @constCast("NULL NAME THIS IS A BUG PLEASE REPORT IT NOW!!"), item);
            },
            else => {
                return codegen.getVariable(name.source orelse @constCast("uhhhhhhhhh this is awkward"));
            },
        }
    }

    fn primaryRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) ParseErrorSet!Handle {
        const token: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
        self.advance();

        return switch (token.tokenType) {
            .leftParen => grouping: {
                const expr = try self.expressionRule(codegen, allocator);

                // the token will be the token following expr
                const current: Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
                if (current.tokenType != .rightParen) {
                    try self.recordErrorTrace(ParsingError.ExpectedClosingParen);
                }
                self.advance();
                break :grouping expr;
            },
            .number => CodeGen.newNumberLit(std.fmt.parseFloat(f64, token.source orelse unreachable) catch 0),
            .string => try codegen.newStringLit(token.source orelse unreachable),
            .kwNil => CodeGen.newNilLit(),
            .kwTrue => comptime CodeGen.newBoolLit(true),
            .kwFalse => comptime CodeGen.newBoolLit(false),
            else => return error.UnexpectedToken,
        };
    }
};
