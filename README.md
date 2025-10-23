interpreter for lox, based off of the book [Crafting Interpreters](https://craftinginterpreters.com/) by Robert Nystrom

The error handling I decided to use was a fair bit interesting.
If we used the built-in errors that Zig has to report errors, then:
- We lose the ability to continue parsing, since returning an error will
  unwind the entire callstack and context. This means you can only handle 1
  parse/compile error at a time, which is very annoying (I deal with this
  all the time when trying to write new features in zlox!)
- We lose the ability to provide context with the errors. The most obvious context
  is a snippet of the offending code, but I'd like to add functionality for analysing
  this context and providing some possible solutions (think of rustc hints) in the future.
What we do instead is that we just pass in an object that represents an error handler (basically
just a stack). If we encounter a non-fatal error, push it onto there and keep chugging along.
This relies on the fact that we will *never* use the generated output if we encounter any error.
Because of this, any rule that **must** return a value in its signature (like fetching a variable
when the variable doesn't exist) can just send back some junk. yay!