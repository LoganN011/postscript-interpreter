{- ============================================================================
   MODULE: PostScript.Primitives.Control
   PATH:   src/PostScript/Primitives/Control.hs

   DESCRIPTION:
     Implements relational comparisons, relational boolean logic, conditional branches,
     and loop primitives.

   RESPONSIBILITIES:
     1. Relational & Boolean Primitives:
        - `eq`, `ne`, `gt`, `ge`, `lt`, `le`: Compare top two objects, push `PSBoolean`.
        - `and`, `or`, `not`, `xor`: Bitwise/boolean operators.
     2. Conditionals:
        - `if`: Pop `PSBlock` and `PSBoolean`. Execute block if `True`.
        - `ifelse`: Pop block2, block1, boolean. Execute block1 if `True`, block2 if `False`.
     3. Iteration & Loops:
        - `repeat`: Pop `PSBlock` and count $n$. Execute block $n$ times.
        - `for`: Pop `PSBlock`, limit, step, initial. Loop from initial to limit by step,
                 pushing current counter value onto stack each iteration before executing block.
        - `loop`, `exit`: Continuous looping and loop breaking mechanism.

   FUNCTIONS TO IMPLEMENT:
     - psEq, psNe, psGt, psGe, psLt, psLe :: Interpreter ()
     - psAnd, psOr, psNot, psXor :: Interpreter ()
     - psIf, psIfelse :: Interpreter ()
     - psRepeat, psFor :: Interpreter ()
     - psLoop, psExit :: Interpreter ()
   ============================================================================ -}