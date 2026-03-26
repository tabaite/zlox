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
};

pub const StmtHandle = u32;
pub const ExprHandle = u32;

pub const VariableAssignment = struct {
    name: []u8,
    val: ExprHandle,
};

pub const Statement = union(enum) {
    declaration: VariableAssignment,
    assignment: VariableAssignment,
    expression: ExprHandle,
};

pub const Expression = union(enum) {
    binary: struct { lhs: ExprHandle, rhs: ExprHandle },
    unary: struct { rhs: ExprHandle },
    literal: union(enum) {
        number: f128,
        string: []u8,
        boolean: bool,
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
    statementList: ArrayList(Statement),
    expressionList: ArrayList(Expression),
    argumentList: ArrayList(ExprHandle),

    pub fn init(alloc: Allocator) AST {
        return .{
            .alloc = alloc,
            .statementList = .initCapacity(alloc, 4096),
            .expressionList = .initCapacity(alloc, 8192),
            .argumentList = .initCapacity(alloc, 4096),
        };
    }
    pub fn deinit(ast: *AST) void {
        const alloc = ast.alloc;

        alloc.free(ast.statementList);
        alloc.free(ast.expressionList);
        alloc.free(ast.argumentList);
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
        const idx: u32 = @truncate(ast.expression.items.len);
        allocatorMust(void, ast.expressionList.append(ast.alloc, expr));
        return idx;
    }
};
