// AST:
// a tree of expression syntax basically
//
// and turn it into this
// functions list:
// [ ... foo ... ]
// v-----
// foo:
// - block: function body block id (number)
//
// statement list:
// [ ... (start) -- (n) -- (end) ... ]
// v-----------------
// n:
// - expr: the expression
// - phi start/end: indicators for phi nodes
//
// we don't actually have any "block" structure because each block scope is a subset of
// each parent block, so it doesn't really make sense
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
const Writer = std.Io.Writer;
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

// fix size l8r
pub const VariableRecord = struct {
    hash: u128,
    iteration: u128,
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

/// The Expression type is the actual storage type for expressions.
/// Since this is exclusively used within the AST, Ranges are employed for references
/// to many other objects within the AST.
pub const Expression = union(enum) {
    // this is too hard lowk
    // We are gonna try to implement efficient phi nodes with dominance frontier checking
    // (see R. Cytron, J. Ferrante, B. Rosen, M. Wegman, and K. Zadeck. Efficiently Computing Static Single Assignment Form and the Control Dependence Graph. ACM Transactions on Programming Languages and Systems, 13(4):451-490, October 1991)
    phi: struct { valExprRange: Range },

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
        argExprRange: Range,
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
    phiValueList: ArrayList(ExprHandle),
    functionList: ArrayList(Function),
    functionArgumentNamesList: ArrayList([]u8),
    statementList: ArrayList(Statement),
    expressionList: ArrayList(Expression),
    argumentList: ArrayList(ExprHandle),

    pub fn init(alloc: Allocator) AST {
        return .{
            .alloc = alloc,
            .phiValueList = allocatorMust(ArrayList(ExprHandle), ArrayList(ExprHandle).initCapacity(alloc, 1024)),
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
        ast.phiValueList.deinit(alloc);
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
                .argExprRange = .{ .start = argStart, .end = argEnd },
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

pub fn printAST(ast: *AST, out: *Writer) !void {
    _ = try out.write("bytecode printing currently not supported due to ast refactoring\n");
    const fns = ast.functionList.items;

    for (fns) |f| {
        try out.print("function \"{s}\" ( ", .{f.name});

        const fargs = ast.functionArgumentNamesList.items[f.argNames.start..f.argNames.end];
        for (fargs) |argname| {
            try out.print("\"{s}\" ", .{argname});
        }
        _ = try out.write(")\n");

        const stmts = ast.statementList.items[f.statements.start..f.statements.end];
        for (stmts) |stmt| {
            _ = try out.write("\t");
            switch (stmt) {
                .assignment => |a| {
                    try out.print("{{ {s} = ", .{a.name});
                    try printASTExpr(ast, a.val, out);
                },
                .declaration => |d| {
                    try out.print("{{ decl {s} = ", .{d.name});
                    try printASTExpr(ast, d.val, out);
                },
                .expression => |e| {
                    _ = try out.write("{ ");
                    try printASTExpr(ast, e, out);
                },
                .funReturn => |r| {
                    _ = try out.write("{ return ");
                    try printASTExpr(ast, r, out);
                },
                .phiScopeStart, .scopeEnd, .scopeStart => {},
            }
            _ = try out.write(" }\n");
        }
    }
}

pub fn printASTExpr(ast: *AST, exprIdx: u32, out: *Writer) !void {
    const expr = ast.expressionList.items[exprIdx];

    switch (expr) {
        .binary => |b| {
            _ = try out.write("( binary ");
            try printASTExpr(ast, b.lhs, out);
            _ = try out.write(" ");
            try printASTExpr(ast, b.rhs, out);
            _ = try out.write(" )");
        },
        .unary => |u| {
            _ = try out.write("( unary ");
            try printASTExpr(ast, u.rhs, out);
            _ = try out.write(" )");
        },
        .functionCall => |f| {
            try out.print("( fn \"{s}\"", .{f.name});
            for (f.argExprRange.start..f.argExprRange.end) |e| {
                try printASTExpr(ast, @intCast(e), out);
                _ = try out.write(", ");
            }
            _ = try out.write(")");
        },
        .variable => |v| try out.print("( {s} )", .{v}),
        .literal => |l| {
            _ = try out.write("( ");
            switch (l) {
                .number => |n| try out.print("{d}", .{n}),
                .string => |s| try out.print("\"{s}\"", .{s}),
                .true => _ = try out.write("TRUE"),
                .false => _ = try out.write("FALSE"),
                .nil => _ = try out.write("NIL"),
            }
            _ = try out.write(" )");
        },
    }
}
