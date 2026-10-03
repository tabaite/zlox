## The grammar language is from the Crafting Interpreters book ([5.1.2](https://craftinginterpreters.com/appendix-i.html)).

i haven't included the lexical grammar but it's all implemented

NOTE: we don't implement the printStmt rule (the author admits it's a hack), but always present a print() function as an alternative.

status legend:
✅ means the rule is fully implemented, strictly including all of its **direct** children

⚠️ means an implementation which is not finished

❌ means not started implementation at all

⬛ means not applicable (for rules which do not have codegen translations)

| parsing status | codegen status | rule name | rule expansion | notes |
| - | - | -------------- | ----------------------------------------------------------------------------------------------- | -------------- |
| ⚠️ | ⚠️ | program      | ( declaration )\* EOF | |
| ⚠️ | ⚠️ | declaration  | ( classDecl \| funDecl \| varDecl \| statement ) | |
| ❌ | ❌ | classDecl    | "class" IDENTIFIER ( "<" IDENTIFIER )? "{" function* "}" | |
| ⚠️ | ⚠️ | funDecl      | "fun" function | the splitting of the funDecl and function rules has not occurred yet as i am not yet working on classes |
| ⚠️ | ⚠️ | function     | IDENTIFIER "(" parameters? ")" ( type )? block | see funDecl. also return type is not vanilla |
| ⚠️ | ⬛ | parameters   | IDENTIFIER ":" type ("," IDENTIFIER ":" type)\* | types are really weird, they have not been updated to be optional (for language compliance) yet. |
| ✅ | ✅ | varDecl      | "var" IDENTIFIER ( ":" type )? ( "=" expression )? ";" | |
| ⚠️ | ⬛ | statement    | ( return \| expression \| block \| ifStmt \| whileStmt \| forStmt ) | |
| ✅ | ⬛ | block        | "{" ( declaration )\* "}" | |
| ✅ | ⬛ | exprStmt     | expression ";" | |
| ❌ | ❌ | forStmt      | "for" "(" ( varDecl \| exprStmt \| ";" ) expression? ";" expression? ")" statement | |
| ✅ | ✅ | returnStmt   | "return" expression ";" | |
| ✅ | ❌ | whileStmt    | "while" "(" expression ")" statement | |
| ✅ | ❌ | ifStmt       | "if" "(" expression ")" statement ( "else" statement ) | |
| ✅ | ⬛ | expression   | assignment | does not really have any codegen to implement |
| ✅ | ✅ | assignment   | IDENTIFIER "=" expression \| logic_or | |
| ✅ | ✅ | logic_or     | logic_and ( "or" logic_and )\* | |
| ✅ | ✅ | logic_and    | equality ( "and" equality )\* | |
| ✅ | ✅ | equality     | comparison ( ( "!=" \| "==" ) comparison )\* | |
| ✅ | ✅ | comparison   | term ( ( ">" \| ">=" \| "<" \| "<=" ) term )\* | |
| ✅ | ✅ | term         | factor ( ( "-" \| "+" ) factor )\* | |
| ✅ | ✅ | factor       | unary ( ( "/" \| "\*" \| "%" ) unary )* | |
| ✅ | ✅ | unary        | ( "!" \| "-" ) unary | call | primary | |
| ✅ | ✅ | call         | IDENTIFIER "(" arguments ")" | i think it's done for now..? there is a basic callstack implementation |
| ✅ | ✅ | arguments    | expression ( "," expression )\* | |
| ✅ | ✅ | primary      | NUMBER \| STRING \| "true" \| "false" \| "nil" \| "(" expression ")" \| IDENTIFIER | |
| ⚠️ | ⚠️ | type         | IDENTIFIER \| "number" \| "bool" \| "string" \| "void" | not vanilla lox, may be depreciated. the IDENTIFIER type (user defined classes) is not implemented. |
