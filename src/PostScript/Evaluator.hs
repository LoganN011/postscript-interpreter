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