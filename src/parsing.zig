const scanning = @import("scanning.zig");
const std = @import("std");
const bytecode = @import("bytecode.zig");

// program        → ( statement )* EOF
// function       → "fun" IDENTIFIER "(" ( (IDENTIFIER ":" type ",")* (IDENTIFIER ":" type) )? ")" ( type )? block
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

const CodeGen = bytecode.BytecodeGenerator;
const Allocator = std.mem.Allocator;
const AnyWriter = std.io.AnyWriter;

const Handle = bytecode.HandledOperand;

const ParseErrorSet = ParsingError || Allocator.Error || bytecode.CompilationError;

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
    lastToken: ?scanning.Token,

    pub fn new(iter: *scanning.TokenIterator) AstParser {
        return .{ .iter = iter, .lastToken = iter.next() };
    }

    // Tries to peek at the token at the position of our parser. Returns null if we are at the end of the list.
    pub fn tryPeek(self: *AstParser) ?scanning.Token {
        return self.lastToken;
    }

    fn advance(self: *AstParser) void {
        self.lastToken = self.iter.next();
    }

    // The way this AST parser works is somewhat simple.
    // Each rule, described by the table above is a function.
    // The function mutates the state of the parser, moving the position forward
    // to the token immediately after the expression it returns.

    pub fn parseAndCompileAll(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !void {
        while (self.tryPeek()) |t| {
            switch (t.tokenType) {
                .leftBrace => try self.blockRule(codegen, allocator),
                .kwFun => try self.functionDeclarationRule(codegen, allocator),
                else => try self.statementRule(codegen, allocator),
            }
        }
    }

    fn functionDeclarationRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !void {
        const fun = self.tryPeek() orelse return ParsingError.ExpectedToken;
        if (fun.tokenType != .kwFun) {
            return ParsingError.ExpectedToken;
        }
        self.advance();
        const funNameT = self.tryPeek() orelse return ParsingError.ExpectedIdentifier;
        if (funNameT.tokenType != .identifier) {
            return ParsingError.ExpectedIdentifier;
        }
        self.advance();
        const open = self.tryPeek() orelse return ParsingError.ExpectedOpeningParen;
        if (open.tokenType != .leftParen) {
            return ParsingError.ExpectedOpeningParen;
        }
        self.advance();

        const ArgFormat = struct {
            name: []u8,
            type: enum {
                number,
                bool,
                string,
            },
        };

        var args: [128]ArgFormat = undefined;
        var argCount: usize = 0;

        while (self.tryPeek()) |t| {
            if (t.tokenType == .rightParen) {
                break;
            }
            const argNameT = self.tryPeek() orelse return ParsingError.ExpectedIdentifier;
            if (argNameT.tokenType != .identifier) {
                return ParsingError.ExpectedIdentifier;
            }
            self.advance();
            const colon = self.tryPeek() orelse return ParsingError.ExpectedType;
            if (colon.tokenType != .colon) {
                return ParsingError.ExpectedType;
            }
            self.advance();
            const argType = self.tryPeek() orelse return ParsingError.ExpectedType;
            const argName = argNameT.source orelse unreachable;
            args[argCount] = switch (argType.tokenType) {
                .tyBool => .{ .name = argName, .type = .bool },
                .tyNum => .{ .name = argName, .type = .number },
                .tyString => .{ .name = argName, .type = .string },
                .tyVoid => return ParsingError.ArgumentCannotBeTypeVoid,
                else => return ParsingError.ExpectedType,
            };
            self.advance();
            if (argCount == 127) {
                return ParsingError.ArgLimit128;
            } else {
                argCount += 1;
            }
        }

        const close = self.tryPeek() orelse return ParsingError.ExpectedClosingParen;
        if (close.tokenType != .rightParen) {
            return ParsingError.ExpectedClosingParen;
        }
        self.advance();
        const retType = self.tryPeek() orelse return self.blockRule(codegen, allocator);
        switch (retType.tokenType) {
            .tyBool => {},
            .tyNum => {},
            .tyString => {},
            .tyVoid => {},
            // Start of function body. We assume this means void.
            .rightBrace => {},
            else => return ParsingError.ExpectedType,
        }
        self.advance();
        const funName = funNameT.source orelse unreachable;
        std.debug.print("DECLARE FUN \"{s}\"\n", .{funName});
        for (0..argCount) |i| {
            const arg = args[i];
            const t = switch (arg.type) {
                .number => "number",
                .string => "string",
                .bool => "bool",
            };
            std.debug.print("  ARG \"{s}\" : {s}\n", .{ arg.name, t });
        }
        // TODO: this block rule contains the function. ideally, we have
        // codegen functions such as enterFunction/exitFunction to manage
        // scoping and stuff
        return self.blockRule(codegen, allocator);
    }

    fn blockRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) ParseErrorSet!void {
        const opening: scanning.Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
        if (opening.tokenType != .leftBrace) {
            return ParsingError.ExpectedOpeningBrace;
        }
        self.advance();
        codegen.enterScope();
        try self.blockBodyRule(codegen, allocator);
        const closing: scanning.Token = self.tryPeek() orelse .{ .tokenType = .invalidChar, .source = null };
        if (closing.tokenType != .rightBrace) {
            return ParsingError.ExpectedClosingBrace;
        }
        codegen.exitScope();
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
        const end = self.tryPeek() orelse scanning.Token{ .tokenType = .invalidChar, .source = null };
        if (end.tokenType != .semicolon) {
            return ParsingError.ExpectedSemicolon;
        }
        self.advance();
    }

    fn declarationRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !Handle {
        const decl = self.tryPeek() orelse return self.expressionRule(codegen, allocator);
        if (decl.tokenType != .kwVar) {
            return self.expressionRule(codegen, allocator);
        }
        self.advance();
        const name = self.tryPeek() orelse return ParsingError.ExpectedIdentifier;
        if (name.tokenType != .identifier) {
            return ParsingError.ExpectedIdentifier;
        }

        self.advance();
        switch ((self.tryPeek() orelse return ParsingError.ExpectedSemicolon).tokenType) {
            .colon => {
                self.advance();

                const typeToken = self.tryPeek() orelse return ParsingError.ExpectedType;
                const varType: bytecode.Type = switch (typeToken.tokenType) {
                    .tyBool => .bool,
                    .tyNum => .number,
                    .tyString => .string,
                    .tyVoid => .nil,
                    .identifier => return ParsingError.CustomTypesNotYetSupported,
                    else => return ParsingError.ExpectedType,
                };

                self.advance();

                const next = self.tryPeek() orelse return ParsingError.ExpectedSemicolon;
                const initialValue: ?Handle = val: {
                    switch (next.tokenType) {
                        .semicolon => break :val null,
                        .equal => {
                            self.advance();
                            break :val try self.expressionRule(codegen, allocator);
                        },
                        else => return ParsingError.ExpectedToken,
                    }
                };
                return try codegen.registerVariable(name.source orelse @constCast("NULLSFEPIRUPWUREWIP"), .{ .provided = .{ .type = varType, .initial = initialValue } });
            },
            .semicolon => return ParsingError.VariableDeclarationMustBeTyped,
            .equal => {
                self.advance();
                return try codegen.registerVariable(name.source orelse @constCast("NULLSFEPIRUPWUREWIP"), .{ .fromValue = try self.expressionRule(codegen, allocator) });
            },
            else => return ParsingError.ExpectedToken,
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
        const opToken = self.tryPeek() orelse return error.ExpectedToken;

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
        const name = self.tryPeek() orelse return self.primaryRule(codegen, allocator);
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

                var args: [32]Handle = undefined;
                var argNums: usize = 0;
                while (self.tryPeek()) |t| {
                    if (t.tokenType == .rightParen) {
                        break;
                    }

                    args[argNums] = try self.expressionRule(codegen, allocator);
                    argNums += 1;

                    const seperator = self.tryPeek() orelse scanning.Token{ .tokenType = .invalidChar, .source = null };
                    if (seperator.tokenType != .comma) {
                        break;
                    }
                    self.advance();
                }

                const endParen = self.tryPeek() orelse return ParsingError.ExpectedClosingBrace;
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
        const token = self.tryPeek() orelse return error.ExpectedToken;
        self.advance();

        if (token.tokenType == .leftParen) {
            const expr = try self.expressionRule(codegen, allocator);

            // the token will be the token following expr
            const current = self.tryPeek() orelse return error.ExpectedToken;
            if (current.tokenType != .rightParen) {
                return error.UnexpectedToken;
            } else {
                self.advance();
                return expr;
            }
        }

        return switch (token.tokenType) {
            .number => CodeGen.newNumberLit(std.fmt.parseFloat(f64, token.source orelse "0") catch 0),
            .string => try codegen.newStringLit(token.source orelse unreachable),
            .kwNil => CodeGen.newNilLit(),
            .kwTrue => comptime CodeGen.newBoolLit(true),
            .kwFalse => comptime CodeGen.newBoolLit(false),
            else => return error.UnexpectedToken,
        };
    }
};
