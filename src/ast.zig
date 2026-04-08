// AST:
// a tree of expression syntax basically
//
// and turn it into this
// functions list:
// [ ... foo ... ]
// v-----
// foo:
// - start: start of statement slice (number)
// - end: end of statement slice (number)
//
// statement list:
// [ ... (start) -- (n) -- (end) ... ]
// v-----------------
// n:
// - expr: the expression
//
// expression list (actual tree part):
// [ ... (expr) -- (expr.inner) ... ]
//         --------------^
//
// expr (union):
//  note:
//  - binary: left ( + | - | * | / | ... ) right
//  - unary: ( ! | - ) right
//  - variable / function call
//
// argument list (just for storing indices of arguments)
// [ ... (argstart) -- (n) -- (argend) ... ]
// yeah it's just a range.
//
// we are planning to depreciate type annotations to stick to the original lox spec

const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub const MAX_ARGS = 256;

pub const Range = struct {
    start: u32,
    end: u32,

    pub const EMPTY: @This() = .{ .start = 0, .end = 0 };
};

pub const NULL_HANDLE = 0;
pub const StmtHandle = u32;
pub const ExprHandle = u32;

pub const Function = struct {
    name: []u8,
    statements: Range,
    argNames: Range,
};

pub const VariableAssignment = struct {
    name: []u8,
    val: ExprHandle,
};

pub const Statement = union(enum) {
    declaration: VariableAssignment,
    assignment: VariableAssignment,
    expression: ExprHandle,
    funReturn: ExprHandle,
};

pub const Expression = union(enum) {
    binary: struct { lhs: ExprHandle, rhs: ExprHandle },
    unary: struct { rhs: ExprHandle },
    literal: union(enum) {
        number: f128,
        string: []u8,
        true,
        false,
        nil,
    },
    variable: []u8,
    functionCall: struct {
        name: []u8,
        argRange: Range,
    },
};

/// That's right. Running out of memory IS a fatal error and you can't change my mind.
/// It's not like the old code did anything except bubble the error up to main anyways.
/// This will allow us to operate with errors as "interupts" without the error space
/// being polluted by OutOfMemory.
pub fn allocatorMust(T: type, result: Allocator.Error!T) T {
    return result catch @panic("Interpreter backend ran out of memory.");
}

pub const AST = struct {
    alloc: Allocator,
    functionList: ArrayList(Function),
    functionArgumentNamesList: ArrayList([]u8),
    statementList: ArrayList(Statement),
    expressionList: ArrayList(Expression),
    argumentList: ArrayList(ExprHandle),

    pub fn init(alloc: Allocator) AST {
        return .{
            .alloc = alloc,
            .functionList = allocatorMust(ArrayList(Function), ArrayList(Function).initCapacity(alloc, 1024)),
            .functionArgumentNamesList = allocatorMust(ArrayList([]u8), ArrayList([]u8).initCapacity(alloc, 4096)),
            .statementList = allocatorMust(ArrayList(Statement), ArrayList(Statement).initCapacity(alloc, 4096)),
            .expressionList = allocatorMust(ArrayList(Expression), ArrayList(Expression).initCapacity(alloc, 8192)),
            .argumentList = allocatorMust(ArrayList(ExprHandle), ArrayList(ExprHandle).initCapacity(alloc, 4096)),
        };
    }
    pub fn deinit(ast: *AST) void {
        const alloc = ast.alloc;

        ast.functionList.deinit(alloc);
        ast.functionArgumentNamesList.deinit(alloc);
        ast.statementList.deinit(alloc);
        ast.expressionList.deinit(alloc);
        ast.argumentList.deinit(alloc);
    }

    pub fn newFunction(ast: *AST, name: []u8, argNames: [][]u8, stmts: Range) void {
        const argNameStart: u32 = @truncate(ast.functionArgumentNamesList.items.len);
        const argNameEnd: u32 = argNameStart + @as(u32, @truncate(argNames.len));
        allocatorMust(void, ast.functionArgumentNamesList.appendSlice(ast.alloc, argNames));

        allocatorMust(void, ast.functionList.append(
            ast.alloc,
            .{
                .name = name,
                .statements = stmts,
                .argNames = .{ .start = argNameStart, .end = argNameEnd },
            },
        ));
    }
    pub fn newStatement(ast: *AST, stmt: Statement) StmtHandle {
        const idx: u32 = @truncate(ast.statementList.items.len);
        allocatorMust(void, ast.statementList.append(ast.alloc, stmt));
        return idx;
    }
    pub fn newFunctionCall(ast: *AST, name: []u8, args: []ExprHandle) ExprHandle {
        const argStart: u32 = @truncate(ast.argumentList.items.len);
        const argEnd = @as(u32, @truncate(args.len)) + argStart;

        allocatorMust(void, ast.argumentList.appendSlice(ast.alloc, args));

        const call: Expression = .{
            .functionCall = .{
                .name = name,
                .argRange = .{ .start = argStart, .end = argEnd },
            },
        };
        return ast.newExpression(call);
    }
    /// Pretty please do not use with a function call expression.
    pub fn newExpression(ast: *AST, expr: Expression) ExprHandle {
        const idx: u32 = @truncate(ast.expressionList.items.len);
        allocatorMust(void, ast.expressionList.append(ast.alloc, expr));
        return idx;
    }
};
