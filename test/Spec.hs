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

import Control.Monad.State (execStateT)
import PostScript.Evaluator (defaultSystemDict, runInterpreter)
import PostScript.Lexer (parsePostScript)
import PostScript.Primitives.Math (psAdd, psDiv, psSub)
import PostScript.Primitives.Stack (psDup, psExch, psRoll)
import PostScript.Types (Interpreter, Object (..), PSState (..), initPSState)
import Test.HUnit
import Test.QuickCheck (Property, ioProperty, isSuccess, quickCheckResult)

-- Helper to run an interpreter action on an initial stack
runWithStack :: [Object] -> Interpreter a -> IO [Object]
runWithStack initialStack action = do
  let s0 = initPSState {operandStack = initialStack}
  finalState <- execStateT action s0
  return (operandStack finalState)

-- Helper to parse and evaluate a full PostScript string
evalString :: String -> IO (Either String [Object])
evalString code = case parsePostScript code of
  Left err -> return (Left ("Parse error: " ++ err))
  Right objs -> do
    let s0 = initPSState {dictStack = [defaultSystemDict]}
    res <- runInterpreter objs s0
    return (fmap operandStack res)

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

-- 3. End-to-End Evaluator & Language Tests
testArithmeticChain :: Test
testArithmeticChain = TestCase $ do
  -- (5 + 3) * 2 - 4 = 12
  res <- evalString "5 3 add 2 mul 4 sub"
  assertEqual "compound math expression" (Right [PSInteger 12]) res

testVariables :: Test
testVariables = TestCase $ do
  res <- evalString "/x 10 def /y 25 def x y add"
  assertEqual "variable definition and lookup" (Right [PSInteger 35]) res

testProcedure :: Test
testProcedure = TestCase $ do
  res <- evalString "/square { dup mul } def 6 square"
  assertEqual "procedure execution" (Right [PSInteger 36]) res

testIfTrue :: Test
testIfTrue = TestCase $ do
  res <- evalString "true { 42 } if"
  assertEqual "if executes block when true" (Right [PSInteger 42]) res

testIfFalse :: Test
testIfFalse = TestCase $ do
  res <- evalString "false { 42 } if"
  assertEqual "if skips block when false" (Right []) res

testIfElse :: Test
testIfElse = TestCase $ do
  res <- evalString "10 20 lt { 1 } { 2 } ifelse"
  assertEqual "ifelse branches correctly" (Right [PSInteger 1]) res

testRepeat :: Test
testRepeat = TestCase $ do
  -- 1 * 2^4 = 16
  res <- evalString "1 4 { 2 mul } repeat"
  assertEqual "repeat loop" (Right [PSInteger 16]) res

testForLoop :: Test
testForLoop = TestCase $ do
  -- 0 + 1 + 2 + 3 + 4 + 5 = 15
  res <- evalString "0 1 1 5 { add } for"
  assertEqual "for loop summation" (Right [PSInteger 15]) res

testStackUnderflow :: Test
testStackUnderflow = TestCase $ do
  res <- evalString "pop"
  case res of
    Left _ -> return ()
    Right _ -> assertFailure "Expected stack underflow error"

testUndefinedSymbol :: Test
testUndefinedSymbol = TestCase $ do
  res <- evalString "nonExistentSymbol"
  case res of
    Left _ -> return ()
    Right _ -> assertFailure "Expected undefined symbol error"

-- 4. QuickCheck Properties
-- Invariant: 'x dup pop' leaves 'x' on the stack
prop_dupPop :: Int -> Property
prop_dupPop n = ioProperty $ do
  res <- evalString (show n ++ " dup pop")
  return (res == Right [PSInteger n])

-- Test Registry
tests :: Test
tests =
  TestList
    [ -- Unit tests for primitives
      TestLabel "testDup" testDup,
      TestLabel "testExch" testExch,
      TestLabel "testRoll" testRoll,
      TestLabel "testAddInt" testAddInt,
      TestLabel "testSub" testSub,
      TestLabel "testDivReal" testDivReal,
      -- End-to-end evaluator tests
      TestLabel "testArithmeticChain" testArithmeticChain,
      TestLabel "testVariables" testVariables,
      TestLabel "testProcedure" testProcedure,
      TestLabel "testIfTrue" testIfTrue,
      TestLabel "testIfFalse" testIfFalse,
      TestLabel "testIfElse" testIfElse,
      TestLabel "testRepeat" testRepeat,
      TestLabel "testForLoop" testForLoop,
      TestLabel "testStackUnderflow" testStackUnderflow,
      TestLabel "testUndefinedSymbol" testUndefinedSymbol
    ]

main :: IO ()
main = do
  putStrLn "=== Running HUnit Test Suite ==="
  counts <- runTestTT tests
  putStrLn "\n=== Running QuickCheck Properties ==="
  qcResult <- quickCheckResult prop_dupPop
  if errors counts + failures counts > 0 || not (isSuccess qcResult)
    then error "One or more tests failed!"
    else putStrLn "All tests passed successfully!"
