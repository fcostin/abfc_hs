`abfc_hs` *work-in-progress*
============================

Port of [abfc](http://github.com/fcostin/abfc) to Haskell, including its own [Parsec](http://www.haskell.org/haskellwiki/Parsec) parser for the macro language.

TODO
----

* finish porting built in function definitions over to codegen
* add support for _arch_ string constants (parse code fragments from external files?)
* refactor data types for internal rep. needs a LOT of cleaning up
* rewrite parser to directly construct Macro instead of ParserMacro
* learn how to eliminate boilerplate for "stateful" bits of computation in Machine, Allocator, Env, Eval, etc. State monad?
