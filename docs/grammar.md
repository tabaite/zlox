## The grammar language is from the Crafting Interpreters book ([5.1.2](https://craftinginterpreters.com/appendix-i.html)).

i haven't included the lexical grammar but it's all implemented

NOTE: we don't implement the printStmt rule (the author admits it's a hack), but always present a print() function as an alternative.

status legend:
"Y" means the rule is fully implemented, strictly including all of its **direct** children

"S" means an implementation which is not finished

"N" means not started implementation at all

"-" means not applicable (for rules which do not have codegen translations)

| parsing status | codegen status | rule name | rule expansion | notes |
| - | - | -------------- | ----------------------------------------------------------------------------------------------- | -------------- |
| S | S | program      | ( declaration )\* EOF | |
| S | S | declaration  | ( classDecl \| funDecl \| varDecl \| statement ) | |
| Y | N | classDecl    | "class" IDENTIFIER ( "<" IDENTIFIER )? "{" function* "}" | |
| S | S | funDecl      | "fun" function | the splitting of the funDecl and function rules has not occurred yet as i am not yet working on classes |
| S | S | function     | IDENTIFIER "(" parameters? ")" ( type )? block | see funDecl. also return type is not vanilla |
| S | - | parameters   | IDENTIFIER ":" type ("," IDENTIFIER)\* | types are really weird, they have not been updated to be optional (for language compliance) yet. |
| Y | Y | varDecl      | "var" IDENTIFIER ( "=" expression )? ";" | |
| S | - | statement    | ( return \| exprStmt \| block \| ifStmt \| whileStmt \| forStmt ) | |
| Y | - | block        | "{" ( declaration )\* "}" | |
| Y | - | exprStmt     | expression ";" | |
| Y | N | forStmt      | "for" "(" ( varDecl \| exprStmt \| ";" ) expression? ";" expression? ")" statement | |
| Y | Y | returnStmt   | "return" expression ";" | |
| Y | N | whileStmt    | "while" "(" expression ")" statement | |
| Y | N | ifStmt       | "if" "(" expression ")" statement ( "else" statement ) | |
| Y | - | expression   | assignment | does not really have any codegen to implement |
| S | Y | assignment   | ( call "." )? IDENTIFIER "=" expression \| logic_or | |
| Y | Y | logic_or     | logic_and ( "or" logic_and )\* | |
| Y | Y | logic_and    | equality ( "and" equality )\* | |
| Y | Y | equality     | comparison ( ( "!=" \| "==" ) comparison )\* | |
| Y | Y | comparison   | term ( ( ">" \| ">=" \| "<" \| "<=" ) term )\* | |
| Y | Y | term         | factor ( ( "-" \| "+" ) factor )\* | |
| Y | Y | factor       | unary ( ( "/" \| "\*" \| "%" ) unary )* | |
| Y | Y | unary        | ( "!" \| "-" ) unary | call | primary | |
| Y | S | call         | primary ( "(" arguments ")" \| "." IDENTIFIER ) | i think it's done for now..? there is a basic callstack implementation |
| Y | Y | arguments    | expression ( "," expression )\* | |
| S | Y | primary      | NUMBER \| STRING \| "true" \| "false" \| "nil" \| "(" expression ")" \| IDENTIFIER \| "super" "." IDENTIFIER | |
