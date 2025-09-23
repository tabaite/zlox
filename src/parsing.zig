const scanning = @import("scanning.zig");
const std = @import("std");
const bytecode = @import("bytecode.zig");

// program        → ( statement )* EOF
// statement      → ( decl | call | expression ) ";"
// decl           → "var" IDENTIFIER ( "=" expression )?
// call           → IDENTIFIER "(" ( ( expression "," )* expression ) ")"
// assignment     → IDENTIFIER "=" expression
// expression     → or
// or             → and "or" and
// and            → equality "and" equality
// equality       → comparison ( ( "!=" | "==" ) comparison )*
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )*
// term           → factor ( ( "-" | "+" ) factor )*
// factor         → unary ( ( "/" | "*" | "%" ) unary )*
// unary          → ( "!" | "-" ) unary
//                | primary
// primary        → NUMBER | STRING | "true" | "false" | "nil"
//                | "(" expression ")"

const CodeGen = bytecode.BytecodeGenerator;
const Allocator = std.mem.Allocator;
const AnyWriter = std.io.AnyWriter;

const Operand = bytecode.HandledOperand;
const Destination = bytecode.Handle;

const ParseErrorSet = ParsingError || Allocator.Error || bytecode.CompilationError;

pub const ParsingError = error{
    UnexpectedToken,
    ExpectedToken,
    ExpectedSemicolon,
    ExpectedClosingBrace,
    ExpectedIdentifier,
    ExpectedExpression,
};

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

pub const Type = enum(u8) {
    number,
    string,
    bool,
    nil,
};

pub const Literal = union(Type) {
    number: f64,
    string: []u8,
    bool: bool,
    nil,
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
        while (self.tryPeek()) |_| {
            try self.statementRule(codegen, allocator);
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

    // It seems really hacky to have this random function determine the location, but it makes sense since it knows whether
    // the result is stored in a variable or not.
    fn declarationRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !Operand {
        const decl = self.tryPeek() orelse {
            const dest = try codegen.pushOperand(@constCast("FROM: DECLARATION!!"), null);
            return self.expressionRule(codegen, dest.asHandle(), allocator);
        };
        if (decl.tokenType != .kwVar) {
            const dest = try codegen.pushOperand(@constCast("FROM: DECLARATION!!"), null);
            return self.expressionRule(codegen, dest.asHandle(), allocator);
        }
        self.advance();
        const name = self.tryPeek() orelse return ParsingError.ExpectedIdentifier;
        if (name.tokenType != .identifier) {
            return ParsingError.ExpectedIdentifier;
        }

        self.advance();
        const continuation = (self.tryPeek() orelse return ParsingError.ExpectedSemicolon);
        switch (continuation.tokenType) {
            .semicolon => {
                return try codegen.registerVariable(decl.source orelse @constCast("idkk"), null);
            },
            .equal => {
                self.advance();

                // TODO: add variable registry
                const dest = try codegen.registerVariable(@constCast("initial declaration!!"), null);

                _ = try self.expressionRule(codegen, dest.asHandle(), allocator);

                return dest;
            },
            else => return ParsingError.ExpectedToken,
        }
    }

    fn expressionRule(self: *AstParser, codegen: *CodeGen, dest: Destination, allocator: Allocator) !Operand {
        return try self.orRule(codegen, dest, allocator);
    }

    // might be the most atrocious function body i've ever written
    fn binaryRule(self: *AstParser, allocator: Allocator, codegen: *CodeGen, dest: Destination, matches: []const TokenToBinaryExpr, previousRule: fn (*AstParser, *CodeGen, Destination, std.mem.Allocator) ParseErrorSet!Operand) ParseErrorSet!Operand {
        var expression = try previousRule(self, codegen, dest, allocator);
        while (self.tryPeek()) |token| {
            const operation = matchTokenToExprOrNull(token.tokenType, matches) orelse break;

            self.advance();

            const right = try previousRule(self, codegen, dest, allocator);

            expression = try codegen.pushBinaryOperation(operation, expression, right, dest);
        }
        return expression;
    }
    fn orRule(self: *AstParser, codegen: *CodeGen, dest: Destination, allocator: Allocator) !Operand {
        const matches = &[_]TokenToBinaryExpr{.{ .key = .kwOr, .value = .bOr }};
        return self.binaryRule(allocator, codegen, dest, matches, andRule);
    }
    fn andRule(self: *AstParser, codegen: *CodeGen, dest: Destination, allocator: Allocator) !Operand {
        const matches = &[_]TokenToBinaryExpr{.{ .key = .kwAnd, .value = .bAnd }};
        return self.binaryRule(allocator, codegen, dest, matches, equalityRule);
    }
    fn equalityRule(self: *AstParser, codegen: *CodeGen, dest: Destination, allocator: Allocator) !Operand {
        const matches = &[_]TokenToBinaryExpr{ .{ .key = .bangEqual, .value = .notEquality }, .{ .key = .equalEqual, .value = .equality } };
        return self.binaryRule(allocator, codegen, dest, matches, comparisonRule);
    }
    fn comparisonRule(self: *AstParser, codegen: *CodeGen, dest: Destination, allocator: Allocator) !Operand {
        const matches = &[_]TokenToBinaryExpr{ .{ .key = .greater, .value = .greater }, .{ .key = .greaterEqual, .value = .greaterEqual }, .{ .key = .less, .value = .less }, .{ .key = .lessEqual, .value = .lessEqual } };
        return self.binaryRule(allocator, codegen, dest, matches, termRule);
    }
    fn termRule(self: *AstParser, codegen: *CodeGen, dest: Destination, allocator: Allocator) !Operand {
        const matches = &[_]TokenToBinaryExpr{ .{ .key = .plus, .value = .add }, .{ .key = .minus, .value = .subtract } };
        return self.binaryRule(allocator, codegen, dest, matches, factorRule);
    }
    fn factorRule(self: *AstParser, codegen: *CodeGen, dest: Destination, allocator: Allocator) !Operand {
        const matches = &[_]TokenToBinaryExpr{ .{ .key = .star, .value = .multiply }, .{ .key = .slash, .value = .divide }, .{ .key = .percent, .value = .modulo } };
        return self.binaryRule(allocator, codegen, dest, matches, unaryRule);
    }
    fn unaryRule(self: *AstParser, codegen: *CodeGen, dest: Destination, allocator: Allocator) !Operand {
        const opToken = self.tryPeek() orelse return error.ExpectedToken;

        const operation: UnaryExprType = switch (opToken.tokenType) {
            .bang => .negateBool,
            .minus => .negate,
            else => return try self.functionCallOrVariableOrAssignmentRule(codegen, dest, allocator),
        };

        self.advance();

        const right = try self.unaryRule(codegen, dest, allocator);

        return try codegen.pushUnaryOperation(operation, right, dest);
    }

    // Calls and variable usages both start with an identifier, so they're combined into one rule.
    fn functionCallOrVariableOrAssignmentRule(self: *AstParser, codegen: *CodeGen, dest: Destination, allocator: Allocator) ParseErrorSet!Operand {
        const name = self.tryPeek() orelse return self.primaryRule(codegen, dest, allocator);
        if (name.tokenType != .identifier and name.tokenType != .kwPrint) {
            return self.primaryRule(codegen, dest, allocator);
        }
        self.advance();
        const startParen = self.tryPeek() orelse {
            return Operand.NIL;
        };

        switch (startParen.tokenType) {
            .leftParen => {
                self.advance();

                var args: [32]Operand = undefined;
                var argNums: usize = 0;
                while (self.tryPeek()) |t| {
                    if (t.tokenType == .rightParen) {
                        break;
                    }

                    args[argNums] = try self.expressionRule(codegen, dest, allocator);
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
                return Operand.NIL;
            },
            .equal => {
                self.advance();

                //TODO: add changing of registered variable
                _ = try self.expressionRule(codegen, dest, allocator);

                //const expr = try allocator.create(Expression);
                //expr.* = Expression{ .assignment = .{ .name = name.source orelse @constCast("NULL NAME!!!"), .value = val } };
                return Operand.NIL;
            },
            else => {
                //const v = try allocator.create(Expression);
                //v.* = Expression{ .variable = .{ .name = name.source orelse @constCast("NULL???") } };

                return Operand.NIL;
            },
        }
    }

    fn primaryRule(self: *AstParser, codegen: *CodeGen, dest: Destination, allocator: Allocator) ParseErrorSet!Operand {
        const token = self.tryPeek() orelse return error.ExpectedToken;
        self.advance();

        if (token.tokenType == .leftParen) {
            const expr = try self.expressionRule(codegen, dest, allocator);

            // the token will be the token following expr
            const current = self.tryPeek() orelse return error.ExpectedToken;
            if (current.tokenType != .rightParen) {
                return error.UnexpectedToken;
            } else {
                self.advance();
                return expr;
            }
        }

        const lit: Literal = switch (token.tokenType) {
            .number => .{ .number = std.fmt.parseFloat(f64, token.source orelse "0") catch 0 },
            .string => .{ .string = token.source orelse unreachable },
            .kwNil => .nil,
            .kwTrue => .{ .bool = true },
            .kwFalse => .{ .bool = false },
            else => return error.UnexpectedToken,
        };
        return try codegen.newLiteral(lit);
    }
};
