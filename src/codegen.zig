// This is the *new* code generation backend to be used with the AST.
// Possibly could include SSA, but that's likely outside the scope of my capabilities.

const ast = @import("ast.zig");

// let's try to make SSA!
// var x = 17; <-- first definition of x (x.1)
//
// if (y > 0) { <-- our parser doesn't yet support control flow :( but it will be evident in the ast
//     x = 12; <-- an assignment to an already existing variable... we need to shove a phi node after then
// }
//
//
// the ssa should be (pseudocode):
// def x.1 = 17;
//
// if_stmt:
//     def x.2 = 12;
//
// def x.3 = phi x.1 x.2
//
const Generation = struct {};
