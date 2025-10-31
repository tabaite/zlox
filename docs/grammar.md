## The grammar language is from the Crafting Interpreters book ([5.1.2](https://craftinginterpreters.com/representing-code.html#enhancing-our-notation)).

program        → ( statement )\* EOF
function       → "fun" IDENTIFIER "(" ( (IDENTIFIER ":" type ",")\* (IDENTIFIER ":" type) )? ")" ( type )? block
arg            → IDENTIFIER ":" type; malformed: IDENTIFIER
block          → "{" ( statement )\* "}"
statement      → ( return | decl | expression ) ";"
return         → "return" expression
declaration    → "var" IDENTIFIER ( ":" type )? ( "=" expression )?
assignment     → IDENTIFIER "=" expression
expression     → or
or             → and "or" and
and            → equality "and" equality
equality       → comparison ( ( "!=" | "==" ) comparison )\*
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )\*
term           → factor ( ( "-" | "+" ) factor )\*
factor         → unary ( ( "/" | "\*" | "%" ) unary )*
unary          → ( "!" | "-" ) unary
               | call | primary
call           → IDENTIFIER "(" ( ( expression "," )\* expression ) ")"
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")"
type           → "number" | "bool" | "string" | "void"