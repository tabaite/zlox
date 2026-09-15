## The grammar language is from the Crafting Interpreters book ([5.1.2](https://craftinginterpreters.com/representing-code.html#enhancing-our-notation)).

\* denotes a rule which has not been updated to be compliant.

program        → ( function )\* EOF
function       → "fun" IDENTIFIER "(" ( (IDENTIFIER ":" type ",")\* (IDENTIFIER ":" type) )? ")" ( type )? block
arg            → IDENTIFIER ":" type; malformed: IDENTIFIER
block          → "{" ( statement )\* "}"
statement    * → ( return | declaration | expression )
exprStmt     * → expression ";"
declaration  * → "var" IDENTIFIER ( ":" type )? ( "=" expression )? ";"
return       * → "return" expression ";"
if           * → "if" "(" expression ")" statement ( "else" statement )
expression   * → assignment
assignment   * → IDENTIFIER "=" expression | or
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
