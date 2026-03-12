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
// expr (union):
//  - grouping: ( inner )
//  - binary: left ( + | - | * | / | ... ) right
//  - unary: ( ! | - ) right
//  - variable / function call
