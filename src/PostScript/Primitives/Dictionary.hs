{- ============================================================================
   MODULE: PostScript.Primitives.Dictionary
   PATH:   src/PostScript/Primitives/Dictionary.hs

   DESCRIPTION:
     Implements PostScript dictionary and variable scoping operations.

   RESPONSIBILITIES:
     1. Variable Definition:
        - `def`: Pop value and key (`PSName`), bind key to value in top dictionary of `dictStack`.
     2. Dictionary Scoping Operations:
        - `dict`: Pop integer capacity $n$, push a new empty `PSDict`.
        - `begin`: Pop `PSDict` off `operandStack` and push onto top of `dictStack`.
        - `end`: Pop top dictionary off `dictStack`.
     3. Dictionary Query Operators:
        - `known`: Check if key exists in dictionary.
        - `load`: Search dictionary stack for key and push its raw value onto operand stack.

   FUNCTIONS TO IMPLEMENT:
     - psDef :: Interpreter ()
     - psDict :: Interpreter ()
     - psBegin :: Interpreter ()
     - psEnd :: Interpreter ()
     - psKnown :: Interpreter ()
     - psLoad :: Interpreter ()
   ============================================================================ -}