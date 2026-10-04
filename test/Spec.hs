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

module Main where

import Control.Monad.State
import Test.HUnit
import PostScript.Types
import PostScript.Environment ()
import PostScript.Primitives.Stack
import PostScript.Primitives.Math

-- Helper to run an interpreter action on an initial stack
runWithStack :: [Object] -> Interpreter a -> IO [Object]
runWithStack initialStack action = do
  let s0 = initPSState { operandStack = initialStack }
  finalState <- execStateT action s0
  return (operandStack finalState)

-- 1. Stack Tests
testDup :: Test
testDup = TestCase $ do
  res <- runWithStack [PSInteger 42] psDup
  assertEqual "dup duplicates top value" [PSInteger 42, PSInteger 42] res

testExch :: Test
testExch = TestCase $ do
  res <- runWithStack [PSInteger 1, PSInteger 2] psExch
  assertEqual "exch swaps top two items" [PSInteger 2, PSInteger 1] res

testRoll :: Test
testRoll = TestCase $ do
  -- PS: 1 2 3 3 1 roll -> shifts top 3 by 1
  -- Stack representation (top at head): [3, 2, 1]
  -- Push n=3, j=1
  res <- runWithStack [PSInteger 1, PSInteger 3, PSInteger 3, PSInteger 2, PSInteger 1] psRoll
  assertEqual "roll top 3 elements by 1" [PSInteger 2, PSInteger 1, PSInteger 3] res

-- 2. Math Tests
testAddInt :: Test
testAddInt = TestCase $ do
  -- Stack: [20, 10] represents "10 20 add"
  res <- runWithStack [PSInteger 20, PSInteger 10] psAdd
  assertEqual "add two integers" [PSInteger 30] res

testSub :: Test
testSub = TestCase $ do
  -- Stack: [3, 10] represents "10 3 sub" -> 7
  res <- runWithStack [PSInteger 3, PSInteger 10] psSub
  assertEqual "sub top two values" [PSInteger 7] res

testDivReal :: Test
testDivReal = TestCase $ do
  -- Stack: [2, 7] represents "7 2 div" -> 3.5
  res <- runWithStack [PSInteger 2, PSInteger 7] psDiv
  assertEqual "div returns real" [PSReal 3.5] res

tests :: Test
tests = TestList
  [ TestLabel "testDup" testDup
  , TestLabel "testExch" testExch
  , TestLabel "testRoll" testRoll
  , TestLabel "testAddInt" testAddInt
  , TestLabel "testSub" testSub
  , TestLabel "testDivReal" testDivReal
  ]

main :: IO ()
main = do
  counts <- runTestTT tests
  if errors counts + failures counts > 0
    then error "Tests failed!"
    else putStrLn "All primitive tests passed!"
