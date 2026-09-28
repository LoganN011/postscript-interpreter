{- ============================================================================
   MODULE: Spec
   PATH:   test/Spec.hs

   DESCRIPTION:
     Automated testing framework suite combining HUnit unit tests and QuickCheck 
     property-based tests for the PostScript interpreter.

   RESPONSIBILITIES:
     1. Unit Tests (HUnit):
        - Test Lexer parsing on edge cases (comments, strings, nested blocks).
        - Test stack math operations (`add`, `sub`, `dup`, `exch`, `roll`).
        - Test control flow (`if`, `ifelse`, `repeat`, `for`).
        - Test dictionary variable bindings (`def`, `begin`, `end`).
     2. Property-Based Tests (QuickCheck):
        - Verify arithmetic properties (e.g. `x y add` equals `y x add`).
        - Verify stack invariants (e.g. `x dup pop` leaves `x` on stack).
        - Verify parse-evaluate consistency on randomized inputs.
     3. Example Script Verification:
        - Test full evaluation of sample `.ps` programs without runtime exceptions.

   TEST GROUPS TO IMPLEMENT:
     - lexerTests :: TestTree / Test
     - mathPrimitiveTests :: TestTree / Test
     - stackPrimitiveTests :: TestTree / Test
     - controlFlowTests :: TestTree / Test
     - graphicsStateTests :: TestTree / Test
     - main :: IO ()
         Runs full test suite and outputs summary results.
   ============================================================================ -}