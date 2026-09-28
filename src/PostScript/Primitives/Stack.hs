{- ============================================================================
   MODULE: PostScript.Primitives.Stack
   PATH:   src/PostScript/Primitives/Stack.hs

   DESCRIPTION:
     Implements standard PostScript operand stack manipulation primitives.

   RESPONSIBILITIES:
     1. Core Stack Operators:
        - `pop`: Discard top item from `operandStack`.
        - `dup`: Duplicate top item.
        - `exch`: Exchange top two items on `operandStack`.
        - `index`: Copy $n$-th item from stack top down to top of stack.
        - `roll`: Rotate top $n$ items by $j$ positions.
        - `copy`: Duplicate top $n$ items on stack.
        - `clear`: Clear all items off `operandStack`.
        - `count`: Push current number of items on `operandStack`.
        - `pstack`: Print current stack contents to stdout (debugging helper).

   FUNCTIONS TO IMPLEMENT:
     - psPop :: Interpreter ()
     - psDup :: Interpreter ()
     - psExch :: Interpreter ()
     - psIndex :: Interpreter ()
     - psRoll :: Interpreter ()
     - psCopy :: Interpreter ()
     - psClear :: Interpreter ()
     - psCount :: Interpreter ()
     - psPstack :: Interpreter ()
   ============================================================================ -}