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
const ArrayList = std.ArrayList;

pub const Range = struct {
    start: u32,
    end: u32,
};

pub const ExprHandle = u32;

pub const VariableAssignment = struct {
    name: []u8,
    val: ExprHandle,
};

pub const Statement = union {
    declaration: VariableAssignment,
    assignment: VariableAssignment,
    expression: ExprHandle,
};

pub const Expression = union(enum) {
    binary: struct { lhs: ExprHandle, rhs: ExprHandle },
    unary: struct { rhs: ExprHandle },
};

pub const AST = struct {
    statementList: ArrayList(Statement),
};
