{- ============================================================================
   MODULE: PostScript.Evaluator
   PATH:   src/PostScript/Evaluator.hs

   DESCRIPTION:
     Core execution engine and dispatch loop. Takes a flat stream of `[Object]` items
     from the lexer and updates the global `PSState` sequentially according to
     PostScript evaluation semantics.

   RESPONSIBILITIES:
     1. Object Evaluation Logic (`evalObject`):
        - Numbers, Booleans, Strings, `PSName`, `PSBlock` -> Push directly onto `operandStack`.
        - `PSSymbol name` -> Look up symbol in `dictStack`:
            * If bound to `PSBlock code`, execute block instructions sequentially (`evalProgram`).
            * If bound to value/primitive object, push or execute accordingly.
            * If not in dictionary, check system built-in table and trigger primitive function.
            * If completely unresolved, throw runtime symbol error.
     2. Program Execution (`evalProgram`):
        - Sequentially map `evalObject` across `[Object]`.
     3. Built-in Primitive Registry:
        - Maintain top-level lookup map matching string keys (e.g., `"add"`, `"dup"`, `"moveto"`)
          to their Haskell `Interpreter ()` monad implementations.

   FUNCTIONS TO IMPLEMENT:
     - runInterpreter :: [Object] -> PSState -> IO (Either String PSState)
         Executes token stream starting from an initial state.
     - evalProgram :: [Object] -> Interpreter ()
         Loops over token list and calls `evalObject`.
     - evalObject :: Object -> Interpreter ()
         Evaluates single object (literal push vs. symbol lookup vs. block execution).
     - executeBuiltin :: String -> Interpreter ()
         Looks up symbol in built-in primitives dictionary table and executes action.
     - defaultSystemDict :: Map String Object
         Initial populating dictionary of all built-in keywords mapped to system definitions.
   ============================================================================ -}