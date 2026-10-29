module PostScript.Evaluator
  ( evalProgram,
    evalObject,
    executeBuiltin,
    defaultSystemDict,
    runInterpreter,
  )
where

import Control.Exception (SomeException, displayException, try)
import Control.Monad.State (execStateT)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import PostScript.Environment (lookupDict, push)
import PostScript.Primitives.Control
  ( psAnd,
    psEq,
    psFor,
    psGe,
    psGt,
    psIf,
    psIfelse,
    psLe,
    psLt,
    psNe,
    psNot,
    psOr,
    psRepeat,
    psXor,
  )
import PostScript.Primitives.Dictionary
  ( psBegin,
    psDef,
    psDict,
    psEnd,
    psKnown,
    psLoad,
  )
import PostScript.Primitives.Math
  ( psAbs,
    psAdd,
    psAtan,
    psCeiling,
    psCos,
    psDiv,
    psExp,
    psFloor,
    psIdiv,
    psLn,
    psLog,
    psMod,
    psMul,
    psNeg,
    psRound,
    psSin,
    psSqrt,
    psSub,
    psTruncate,
  )
import PostScript.Primitives.Stack
  ( psClear,
    psCopy,
    psCount,
    psDup,
    psExch,
    psIndex,
    psPop,
    psPstack,
    psRoll,
  )
import PostScript.Types (Interpreter, Object (..), PSState (..))

evalProgram :: [Object] -> Interpreter ()
evalProgram = mapM_ evalObject

-- Evaluates a single PostScript object according to PostScript semantics
evalObject :: Object -> Interpreter ()
evalObject (PSSymbol name) = do
  mVal <- lookupDict name
  case mVal of
    Just (PSBlock code) -> evalProgram code
    Just val -> push val
    Nothing -> executeBuiltin name
evalObject obj = push obj

-- Initial system dictionary containing predefined standard constants
defaultSystemDict :: Map String Object
defaultSystemDict =
  Map.fromList
    [ ("true", PSBoolean True),
      ("false", PSBoolean False)
    ]

--
executeBuiltin :: String -> Interpreter ()
executeBuiltin name = case name of
  -- Stack Primitives
  "pop" -> psPop
  "dup" -> psDup
  "exch" -> psExch
  "index" -> psIndex
  "roll" -> psRoll
  "copy" -> psCopy
  "clear" -> psClear
  "count" -> psCount
  "pstack" -> psPstack
  -- Math Primitives
  "add" -> psAdd
  "sub" -> psSub
  "mul" -> psMul
  "div" -> psDiv
  "idiv" -> psIdiv
  "mod" -> psMod
  "neg" -> psNeg
  "abs" -> psAbs
  "ceiling" -> psCeiling
  "floor" -> psFloor
  "round" -> psRound
  "truncate" -> psTruncate
  "sqrt" -> psSqrt
  "atan" -> psAtan
  "cos" -> psCos
  "sin" -> psSin
  "exp" -> psExp
  "ln" -> psLn
  "log" -> psLog
  -- Dictionary Primitives
  "def" -> psDef
  "dict" -> psDict
  "begin" -> psBegin
  "end" -> psEnd
  "known" -> psKnown
  "load" -> psLoad
  -- Relational & Boolean Primitives
  "eq" -> psEq
  "ne" -> psNe
  "gt" -> psGt
  "ge" -> psGe
  "lt" -> psLt
  "le" -> psLe
  "and" -> psAnd
  "or" -> psOr
  "not" -> psNot
  "xor" -> psXor
  -- Control Flow Primitives (passing `evalProgram` for recursive execution)
  "if" -> psIf evalProgram
  "ifelse" -> psIfelse evalProgram
  "repeat" -> psRepeat evalProgram
  "for" -> psFor evalProgram
  -- Unknown / Undefined
  _ -> error ("Undefined symbol: " ++ name)

-- Top-level runner that executes an object stream from an initial state
runInterpreter :: [Object] -> PSState -> IO (Either String PSState)
runInterpreter objs s0 = do
  result <- try (execStateT (evalProgram objs) s0) :: IO (Either SomeException PSState)
  return
    ( case result of
        Left ex -> Left (displayException ex)
        Right st -> Right st
    )