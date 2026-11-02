module Main (main) where

import PostScript.Evaluator (defaultSystemDict, runInterpreter)
import PostScript.Lexer (parsePostScript)
import PostScript.Types (Object (..), PSState (..), initPSState)
import System.Exit (exitFailure, exitSuccess)
import Test.QuickCheck


-- | Parse and evaluate a raw PostScript string from the root system state.
evalScript :: String -> IO (Either String [Object])
evalScript code = case parsePostScript code of
  Left err -> pure (Left ("Parse error: " ++ err))
  Right objs -> do
    let s0 = initPSState {dictStack = [defaultSystemDict]}
    res <- runInterpreter objs s0
    pure (fmap operandStack res)

-- | Read a .ps file from disk and execute it to completion.
evalFile :: FilePath -> IO (Either String [Object])
evalFile path = do
  source <- readFile path
  evalScript source


-- Stack: 'x dup pop' leaves 'x' on the stack
prop_dupPop :: Int -> Property
prop_dupPop x = ioProperty $ do
  res <- evalScript (show x ++ " dup pop")
  pure (res == Right [PSInteger x])

-- Stack: 'x y exch exch' preserves the original stack order
prop_exchTwice :: Int -> Int -> Property
prop_exchTwice x y = ioProperty $ do
  res <- evalScript (show x ++ " " ++ show y ++ " exch exch")
  pure (res == Right [PSInteger y, PSInteger x])

-- Stack: 'dup add' is equivalent to doubling
prop_dupAddDoubles :: Int -> Property
prop_dupAddDoubles x = ioProperty $ do
  res <- evalScript (show x ++ " dup add")
  pure (res == Right [PSInteger (x * 2)])

-- Stack: 'count' accurately reports number of pushed items
prop_countMatches :: [Int] -> Property
prop_countMatches xs =
  let n = length xs
      script = unwords (map show xs) ++ " count"
   in ioProperty $ do
        res <- evalScript script
        pure (case res of
                Right (PSInteger c : _) -> c == n
                _                       -> False)

-- Stack: 'clear' empties the entire operand stack
prop_clearEmpties :: [Int] -> Property
prop_clearEmpties xs =
  let script = unwords (map show xs) ++ " clear"
   in ioProperty $ do
        res <- evalScript script
        pure (res == Right [])

-- Math: Commutativity of addition (x + y == y + x)
prop_addCommutative :: Int -> Int -> Property
prop_addCommutative x y = ioProperty $ do
  res1 <- evalScript (show x ++ " " ++ show y ++ " add")
  res2 <- evalScript (show y ++ " " ++ show x ++ " add")
  pure (res1 == Right [PSInteger (x + y)] && res1 == res2)

-- Math: Commutativity of multiplication (x * y == y * x)
prop_mulCommutative :: Int -> Int -> Property
prop_mulCommutative x y = ioProperty $ do
  res1 <- evalScript (show x ++ " " ++ show y ++ " mul")
  res2 <- evalScript (show y ++ " " ++ show x ++ " mul")
  pure (res1 == Right [PSInteger (x * y)] && res1 == res2)

-- Math: Subtraction and addition are inverses: (x - y) + y == x
prop_subInverse :: Int -> Int -> Property
prop_subInverse x y = ioProperty $ do
  res <- evalScript (show x ++ " " ++ show y ++ " sub " ++ show y ++ " add")
  pure (res == Right [PSInteger x])

-- Math: Double negation is the identity
prop_negInvolution :: Int -> Property
prop_negInvolution x = ioProperty $ do
  res <- evalScript (show x ++ " neg neg")
  pure (res == Right [PSInteger x])

-- Math: Absolute value is never negative
prop_absNonNegative :: Int -> Property
prop_absNonNegative x = ioProperty $ do
  res <- evalScript (show x ++ " abs")
  pure (res == Right [PSInteger (abs x)])

-- Math: Euclidean division identity: (x idiv y) * y + (x mod y) == x
prop_idivModIdentity :: Int -> NonZero Int -> Property
prop_idivModIdentity x (NonZero y) = ioProperty $ do
  let script = show x ++ " " ++ show y ++ " idiv " ++ show y ++ " mul "
            ++ show x ++ " " ++ show y ++ " mod add"
  res <- evalScript script
  pure (res == Right [PSInteger x])

-- Control: 'ifelse' selects the first branch on true
prop_ifelseTrue :: Int -> Int -> Property
prop_ifelseTrue x y = ioProperty $ do
  res <- evalScript ("true { " ++ show x ++ " } { " ++ show y ++ " } ifelse")
  pure (res == Right [PSInteger x])

-- Control: 'ifelse' selects the second branch on false
prop_ifelseFalse :: Int -> Int -> Property
prop_ifelseFalse x y = ioProperty $ do
  res <- evalScript ("false { " ++ show x ++ " } { " ++ show y ++ " } ifelse")
  pure (res == Right [PSInteger y])

-- Control: 'repeat' executes exactly n iterations
prop_repeatCount :: NonNegative (Small Int) -> Property
prop_repeatCount (NonNegative (Small n)) = ioProperty $ do
  res <- evalScript ("0 " ++ show n ++ " { 1 add } repeat")
  pure (res == Right [PSInteger n])

-- Scoping: 'def' followed by identifier evaluates to the bound value
prop_defLookup :: Int -> Property
prop_defLookup x = ioProperty $ do
  res <- evalScript ("/x " ++ show x ++ " def x")
  pure (res == Right [PSInteger x])

-- Relational: Equality is reflexive (x eq x == true)
prop_eqReflexive :: Int -> Property
prop_eqReflexive x = ioProperty $ do
  res <- evalScript (show x ++ " " ++ show x ++ " eq")
  pure (res == Right [PSBoolean True])



data ProgramTest = ProgramTest
  { testTitle    :: String
  , programPath  :: FilePath
  , expectedStack:: [Object]
  }

testPrograms :: [ProgramTest]
testPrograms =
  [ ProgramTest
      { testTitle     = "Fibonacci, Lucas numbers & algebraic identity"
      , programPath   = "test/programs/fibonacci.ps"
      , expectedStack = [PSBoolean True, PSInteger 33, PSInteger 55]
      }
  , ProgramTest
      { testTitle     = "Prime sieve, primality check & summation"
      , programPath   = "test/programs/primes.ps"
      , expectedStack = [PSBoolean False, PSBoolean True, PSInteger 129, PSInteger 10]
      }
  , ProgramTest
      { testTitle     = "Collatz 3n+1 sequence & Horner's polynomial"
      , programPath   = "test/programs/collatz_poly.ps"
      , expectedStack = [PSInteger 22, PSInteger 111, PSInteger 8]
      }
  , ProgramTest
      { testTitle     = "Dictionary scoping, shadowing & 'known'/'load'"
      , programPath   = "test/programs/dict_scope.ps"
      , expectedStack = [ PSBoolean False, PSInteger 1, PSInteger 2
                        , PSInteger 100, PSInteger 300, PSInteger 999
                        , PSInteger 42, PSInteger 1199
                        ]
      }
  , ProgramTest
      { testTitle     = "Operand stack indexing, copying, and multi-element roll"
      , programPath   = "test/programs/stack_ops.ps"
      , expectedStack = [ PSInteger 5, PSInteger 340, PSInteger 40
                        , PSInteger 30, PSInteger 10, PSInteger 20
                        ]
      }
  , ProgramTest
      { testTitle     = "Trigonometry, logarithms, rounding family & division"
      , programPath   = "test/programs/math_advanced.ps"
      , expectedStack = [ PSInteger (-15), PSInteger 42, PSReal 3.4, PSInteger 2, PSInteger 3
                        , PSReal (-3.0), PSReal 3.0, PSReal 3.0, PSReal 4.0, PSReal (-4.0)
                        , PSReal 3.0, PSReal (-3.0), PSReal 4.0, PSReal 0.0, PSReal 3.0
                        , PSReal 256.0, PSReal 90.0, PSReal 45.0, PSBoolean True, PSBoolean True
                        , PSReal 13.0, PSReal 5.0
                        ]
      }
  , ProgramTest
      { testTitle     = "Euclidean GCD, LCM, recursive Factorial & Combinatorics"
      , programPath   = "test/programs/euclid_gcd.ps"
      , expectedStack = [ PSInteger 243, PSInteger 210, PSInteger 35, PSInteger 15
                        , PSInteger 5040, PSInteger 720, PSInteger 60, PSInteger 36
                        , PSInteger 12, PSInteger 35, PSInteger 6
                        ]
      }
  , ProgramTest
      { testTitle     = "Nested conditionals, De Morgan laws & nested for-loops"
      , programPath   = "test/programs/control_flow.ps"
      , expectedStack = [ PSInteger 31, PSInteger 30, PSInteger 60
                        , PSBoolean True, PSBoolean True, PSBoolean True
                        , PSBoolean True, PSBoolean True
                        , PSInteger 1, PSInteger 0, PSInteger (-1)
                        ]
      }
  ]

-- Runs a single program test and prints its result.
runProgramTest :: ProgramTest -> IO Bool
runProgramTest t = do
  putStr ("  * " ++ testTitle t ++ " ... ")
  res <- evalFile (programPath t)
  case res of
    Right stack
      | stack == expectedStack t -> do
          putStrLn "PASS"
          pure True
      | otherwise -> do
          putStrLn "FAIL"
          putStrLn $ "      Expected: " ++ show (expectedStack t)
          putStrLn $ "      Actual:   " ++ show stack
          pure False
    Left err -> do
      putStrLn "ERROR"
      putStrLn $ "      " ++ err
      pure False


main :: IO ()
main = do
  putStrLn "\n--- Section 1: Property-Based Testing (QuickCheck) ---"
  let qcProps =
        [ ("Stack: dup pop preserves top value", quickCheckResult (withNumTests 100 prop_dupPop))
        , ("Stack: exch twice preserves order", quickCheckResult (withNumTests 100 prop_exchTwice))
        , ("Stack: dup add doubles value", quickCheckResult (withNumTests 100 prop_dupAddDoubles))
        , ("Stack: count matches pushed items", quickCheckResult (withNumTests 100 prop_countMatches))
        , ("Stack: clear empties operand stack", quickCheckResult (withNumTests 100 prop_clearEmpties))
        , ("Math: addition is commutative", quickCheckResult (withNumTests 100 prop_addCommutative))
        , ("Math: multiplication is commutative", quickCheckResult (withNumTests 100 prop_mulCommutative))
        , ("Math: subtraction and addition are inverses", quickCheckResult (withNumTests 100 prop_subInverse))
        , ("Math: double negation is identity", quickCheckResult (withNumTests 100 prop_negInvolution))
        , ("Math: absolute value is non-negative", quickCheckResult (withNumTests 100 prop_absNonNegative))
        , ("Math: integer division and modulo identity", quickCheckResult (withNumTests 100 prop_idivModIdentity))
        , ("Control: ifelse evaluates true branch", quickCheckResult (withNumTests 100 prop_ifelseTrue))
        , ("Control: ifelse evaluates false branch", quickCheckResult (withNumTests 100 prop_ifelseFalse))
        , ("Control: repeat executes n times", quickCheckResult (withNumTests 100 prop_repeatCount))
        , ("Scoping: variable definition and lookup", quickCheckResult (withNumTests 100 prop_defLookup))
        , ("Relational: equality is reflexive", quickCheckResult (withNumTests 100 prop_eqReflexive))
        ]

  qcResults <- mapM (\(lbl, action) -> putStrLn ("  [QC] " ++ lbl) >> action) qcProps
  let allQcPassed = all isSuccess qcResults

  putStrLn "\n--- Section 2: PostScript Program Integration Tests ---"
  programResults <- mapM runProgramTest testPrograms
  let allProgramsPassed = and programResults

  if allQcPassed && allProgramsPassed
    then do
      putStrLn "RESULT: All QuickCheck properties and PostScript programs PASSED!"
      exitSuccess
    else do
      putStrLn "RESULT: One or more tests FAILED!"
      exitFailure
