## The grammar language is from the Crafting Interpreters book ([5.1.2](https://craftinginterpreters.com/representing-code.html#enhancing-our-notation)).

program        → ( function )\* EOF
function       → "fun" IDENTIFIER "(" ( (IDENTIFIER ":" type ",")\* (IDENTIFIER ":" type) )? ")" ( type )? block
arg            → IDENTIFIER ":" type; malformed: IDENTIFIER
block          → "{" ( line )\* "}"
line           → ( return | expression | assignment | declaration ) ";"
statement      → ( return | expression | assignment ) ";"
declaration    → "var" IDENTIFIER ( ":" type )? ( "=" expression )? ";"
return         → "return" expression
assignment     → IDENTIFIER "=" expression
expression     → or
or             → and ( "or" and )\*
and            → equality ( "and" equality )\*
equality       → comparison ( ( "!=" | "==" ) comparison )\*
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )\*
term           → factor ( ( "-" | "+" ) factor )\*
factor         → unary ( ( "/" | "\*" | "%" ) unary )*
unary          → ( "!" | "-" ) unary
               | call | primary
call           → IDENTIFIER "(" ( ( expression "," )\* expression ) ")"
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" | IDENTIFIER
type           → "number" | "bool" | "string" | "void"
