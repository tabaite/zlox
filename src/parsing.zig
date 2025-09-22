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

const ParseErrorSet = ParsingError || Allocator.Error || bytecode.CompilationError;

pub const ParsingError = error{
    UnexpectedToken,
    ExpectedToken,
    ExpectedSemicolon,
    ExpectedOpeningBrace,
    ExpectedClosingBrace,
    ExpectedIdentifier,
    ExpectedExpression,
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

pub const Block = struct { contents: []BlockContent };

pub const BlockContent = union(enum) { stmt: Statement, block: *Block };

pub const Statement = struct {
    expr: *Expression,
};

pub const Expression = union(enum) {
    declaration: struct {
        name: []u8,
        value: ?*Expression,
    },
    assignment: struct {
        name: []u8,
        value: *Expression,
    },
    functionCall: struct {
        name: []u8,
        args: []*Expression,
    },
    variable: struct {
        name: []u8,
    },
    literal: Literal,
    unary: struct {
        operation: UnaryExprType,
        expr: *Expression,
    },
    binary: struct {
        operation: BinaryExprType,
        left: *Expression,
        right: *Expression,
    },
    grouping: struct { expr: *Expression },
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

    pub fn programTree(self: *AstParser, allocator: std.mem.Allocator) !*Block {
        // The global program scope doesn't include braces around it
        var list = std.ArrayList(BlockContent).init(allocator);
        while (self.tryPeek()) |t| {
            const item = it: {
                if (t.tokenType == .leftBrace) {
                    break :it BlockContent{ .block = try self.blockRule(allocator) };
                } else {
                    break :it BlockContent{ .stmt = try self.statementRule(allocator) };
                }
            };

            try list.append(item);
        }
        const block = try allocator.create(Block);
        block.* = Block{ .contents = list.items };
        return block;
    }

    pub fn nextStatement(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !?Statement {
        if (self.tryPeek() == null) {
            return null;
        }

        return try self.statementRule(codegen, allocator);
    }

    fn blockRule(self: *AstParser, allocator: std.mem.Allocator) !*Block {
        const start = self.tryPeek() orelse {
            const block = try allocator.create(Block);
            block.* = Block{ .contents = &[_]BlockContent{} };
            return block;
        };
        if (start.tokenType != .leftBrace) {
            return ParsingError.ExpectedOpeningBrace;
        }

        self.advance();
        var list = std.ArrayList(BlockContent).init(allocator);
        while (self.tryPeek()) |t| {
            if (t.tokenType == .rightBrace) {
                break;
            }

            const item = it: {
                if (t.tokenType == .leftBrace) {
                    break :it BlockContent{ .block = try self.blockRule(allocator) };
                } else {
                    break :it BlockContent{ .stmt = try self.statementRule(allocator) };
                }
            };

            try list.append(item);
        }
        const block = try allocator.create(Block);
        block.* = Block{ .contents = list.items };

        const end = self.tryPeek() orelse return ParsingError.ExpectedClosingBrace;
        if (end.tokenType != .rightBrace) {
            return ParsingError.ExpectedClosingBrace;
        }
        self.advance();

        return block;
    }

    fn statementRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !Statement {
        const expression = try self.declarationRule(codegen, allocator);
        const end = self.tryPeek() orelse scanning.Token{ .tokenType = .invalidChar, .source = null };
        if (end.tokenType != .semicolon) {
            return ParsingError.ExpectedSemicolon;
        }
        const statement = Statement{ .expr = expression };
        self.advance();
        return statement;
    }

    fn declarationRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !*Expression {
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
        const continuation = (self.tryPeek() orelse return ParsingError.ExpectedSemicolon);
        switch (continuation.tokenType) {
            .semicolon => {
                const expr = try allocator.create(Expression);
                expr.* = Expression{ .declaration = .{ .name = name.source orelse @constCast("NULL NAME???"), .value = null } };
                return expr;
            },
            .equal => {
                self.advance();
                const result = try self.expressionRule(codegen, allocator);

                const expr = try allocator.create(Expression);
                expr.* = Expression{ .declaration = .{ .name = name.source orelse @constCast("NULL NAME???"), .value = result } };
                return expr;
            },
            else => return ParsingError.ExpectedToken,
        }
    }

    fn expressionRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !*Expression {
        return try self.orRule(codegen, allocator);
    }

    // might be the most atrocious function body i've ever written
    fn binaryRule(self: *AstParser, allocator: Allocator, codegen: *CodeGen, matches: []const TokenToBinaryExpr, previousRule: fn (*AstParser, *CodeGen, std.mem.Allocator) ParseErrorSet!*Expression) ParseErrorSet!*Expression {
        var expression = try previousRule(self, codegen, allocator);
        while (self.tryPeek()) |token| {
            const operation = matchTokenToExprOrNull(token.tokenType, matches) orelse break;

            self.advance();

            const right = try previousRule(self, codegen, allocator);

            const newRoot = try allocator.create(Expression);

            newRoot.* = Expression{ .binary = .{ .left = expression, .right = right, .operation = operation } };
            try codegen.pushBinaryOperation(operation, .NIL, .NIL);

            expression = newRoot;
        }
        return expression;
    }
    fn orRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !*Expression {
        const matches = &[_]TokenToBinaryExpr{.{ .key = .kwOr, .value = .bOr }};
        return self.binaryRule(allocator, codegen, matches, andRule);
    }
    fn andRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !*Expression {
        const matches = &[_]TokenToBinaryExpr{.{ .key = .kwAnd, .value = .bAnd }};
        return self.binaryRule(allocator, codegen, matches, equalityRule);
    }
    fn equalityRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !*Expression {
        const matches = &[_]TokenToBinaryExpr{ .{ .key = .bangEqual, .value = .notEquality }, .{ .key = .equalEqual, .value = .equality } };
        return self.binaryRule(allocator, codegen, matches, comparisonRule);
    }
    fn comparisonRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !*Expression {
        const matches = &[_]TokenToBinaryExpr{ .{ .key = .greater, .value = .greater }, .{ .key = .greaterEqual, .value = .greaterEqual }, .{ .key = .less, .value = .less }, .{ .key = .lessEqual, .value = .lessEqual } };
        return self.binaryRule(allocator, codegen, matches, termRule);
    }
    fn termRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !*Expression {
        const matches = &[_]TokenToBinaryExpr{ .{ .key = .plus, .value = .add }, .{ .key = .minus, .value = .subtract } };
        return self.binaryRule(allocator, codegen, matches, factorRule);
    }
    fn factorRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !*Expression {
        const matches = &[_]TokenToBinaryExpr{ .{ .key = .star, .value = .multiply }, .{ .key = .slash, .value = .divide }, .{ .key = .percent, .value = .modulo } };
        return self.binaryRule(allocator, codegen, matches, unaryRule);
    }
    fn unaryRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) !*Expression {
        const opToken = self.tryPeek() orelse return error.ExpectedToken;

        const operation: UnaryExprType = switch (opToken.tokenType) {
            .bang => .negateBool,
            .minus => .negate,
            else => return try self.functionCallOrVariableOrAssignmentRule(codegen, allocator),
        };

        self.advance();

        const right = try self.unaryRule(codegen, allocator);

        const newRoot = try allocator.create(Expression);
        newRoot.* = Expression{ .unary = .{ .operation = operation, .expr = right } };
        try codegen.pushUnaryOperation(operation, .NIL);

        return newRoot;
    }

    // Calls and variable usages both start with an identifier, so they're combined into one rule.
    fn functionCallOrVariableOrAssignmentRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) ParseErrorSet!*Expression {
        const name = self.tryPeek() orelse return self.primaryRule(codegen, allocator);
        if (name.tokenType != .identifier and name.tokenType != .kwPrint) {
            return self.primaryRule(codegen, allocator);
        }
        self.advance();
        const startParen = self.tryPeek() orelse {
            const v = try allocator.create(Expression);

            v.* = Expression{ .variable = .{ .name = name.source orelse @constCast("NULL???") } };

            return v;
        };

        switch (startParen.tokenType) {
            .leftParen => {
                self.advance();

                var args: [32]*Expression = undefined;
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
                const expr = try allocator.create(Expression);
                const fnName = if (name.tokenType == .kwPrint) @constCast(VERYBADPRINTFUNCTIONNAME) else name.source orelse @constCast("NULL TOKEN !!");
                expr.* = Expression{ .functionCall = .{ .name = fnName, .args = try allocator.dupe(*Expression, args[0..argNums]) } };
                self.advance();
                return expr;
            },
            .equal => {
                self.advance();
                const val = try self.expressionRule(codegen, allocator);

                const expr = try allocator.create(Expression);
                expr.* = Expression{ .assignment = .{ .name = name.source orelse @constCast("NULL NAME!!!"), .value = val } };
                return expr;
            },
            else => {
                const v = try allocator.create(Expression);

                v.* = Expression{ .variable = .{ .name = name.source orelse @constCast("NULL???") } };

                return v;
            },
        }
    }

    fn primaryRule(self: *AstParser, codegen: *CodeGen, allocator: Allocator) ParseErrorSet!*Expression {
        const token = self.tryPeek() orelse return error.ExpectedToken;
        self.advance();

        if (token.tokenType == .leftParen) {
            const expr = try self.expressionRule(codegen, allocator);

            // the token will be the token following expr
            const current = self.tryPeek() orelse return error.ExpectedToken;
            if (current.tokenType != .rightParen) {
                return error.UnexpectedToken;
            } else {
                const group = try allocator.create(Expression);
                group.* = Expression{ .grouping = .{ .expr = expr } };
                self.advance();
                return group;
            }
        }

        const primary = try allocator.create(Expression);
        primary.* = lit: switch (token.tokenType) {
            .number => {
                const literal = Expression{ .literal = .{ .number = std.fmt.parseFloat(f64, token.source orelse "0") catch 0 } };
                break :lit literal;
            },
            .string => {
                const literal = Expression{ .literal = .{ .string = token.source orelse unreachable } };
                break :lit literal;
            },
            .kwNil => break :lit Expression{ .literal = .nil },
            .kwTrue => break :lit Expression{ .literal = .{ .bool = true } },
            .kwFalse => break :lit Expression{ .literal = .{ .bool = false } },
            else => return error.UnexpectedToken,
        };
        _ = try codegen.pushLiteral(primary.literal);
        return primary;
    }
};

pub inline fn printBlock(blk: *Block, out: std.io.AnyWriter) !void {
    try printBlockI(blk, 0, out);
}

fn printBlockI(blk: *Block, ident: usize, out: std.io.AnyWriter) !void {
    for (0..ident * 2) |_| {
        _ = try out.write(" ");
    }
    _ = try out.write("{\n");
    for (blk.contents) |i| switch (i) {
        .stmt => |s| try printStatement(s, ident + 1, out),
        .block => |b| try printBlockI(b, ident + 1, out),
    };
    for (0..ident * 2) |_| {
        _ = try out.write(" ");
    }
    _ = try out.write("}\n");
}

pub fn printStatement(stmt: Statement, ident: usize, out: std.io.AnyWriter) !void {
    for (0..ident * 2) |_| {
        _ = try out.write(" ");
    }
pub fn printStatement(stmt: Statement, out: AnyWriter) !void {
    try printExpression(stmt.expr, out);
    _ = try out.write("\n");
}

pub fn printExpression(expr: *Expression, out: AnyWriter) !void {
    switch (expr.*) {
        .assignment => |a| {
            try out.print("SET \"{s}\" TO ", .{a.name});
            try printExpression(a.value, out);
        },
        .declaration => |d| {
            if (d.value == null) {
                _ = try out.print("declare \"{s}\"", .{d.name});
            } else {
                _ = try out.print("declare \"{s}\" = ", .{d.name});

                // Given a VALID AST, this cannot recurse infinitely, as checking the value of
                // a declaration starts at the call rule (a declaration cannot be declared to be another declaration)
                try printExpression(d.value orelse unreachable, out);
            }
        },
        .variable => |v| try out.print("USE \"{s}\"", .{v.name}),
        .functionCall => |f| {
            try out.print("CALL \"{s}\" (", .{f.name});
            for (f.args) |arg| {
                try printExpression(arg, out);
                _ = try out.write(", ");
            }
            _ = try out.write(")");
        },
        .literal => |l| switch (l) {
            .number => |num| try out.print("{d}", .{num}),
            .string => |str| try out.print("\"{s}\"", .{str}),
            .bool => |b| if (b) {
                _ = try out.write("true");
            } else {
                _ = try out.write("false");
            },
            .nil => _ = try out.write("nil"),
        },
        .unary => |u| {
            _ = switch (u.operation) {
                .negate => try out.write("(- "),
                .negateBool => try out.write("(! "),
            };
            try printExpression(u.expr, out);
            _ = try out.write(")");
        },
        .binary => |b| {
            _ = switch (b.operation) {
                .equality => try out.write("(== "),
                .notEquality => try out.write("(!= "),
                .greater => try out.write("(> "),
                .greaterEqual => try out.write("(>= "),
                .less => try out.write("(< "),
                .lessEqual => try out.write("(<= "),
                .add => try out.write("(+ "),
                .subtract => try out.write("(- "),
                .multiply => try out.write("(* "),
                .divide => try out.write("(/ "),
                .modulo => try out.write("(% "),
                .bAnd => try out.write("(and "),
                .bOr => try out.write("(or "),
            };
            try printExpression(b.left, out);
            _ = try out.write(" ");
            try printExpression(b.right, out);
            _ = try out.write(")");
        },
        .grouping => |g| {
            _ = try out.write("(group ");
            try printExpression(g.expr, out);
            _ = try out.write(")");
        },
    }
}
