module PostScript.Evaluator
  ( evalProgram,
    evalObject,
    executeBuiltin,
    defaultSystemDict,
    runInterpreter,
  )
where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (unless)
import Control.Monad.State (execStateT, gets)
import PostScript.Environment (lookupDict, push)
import PostScript.Primitives.Control
  ( psAnd,
    psBind,
    psEq,
    psExec,
    psExit,
    psFor,
    psGe,
    psGt,
    psIf,
    psIfelse,
    psLe,
    psLoop,
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
import PostScript.Primitives.Graphics.Path
  ( psArc,
    psArcn,
    psArcto,
    psClosepath,
    psCurrentpoint,
    psCurveto,
    psFlattenpath,
    psLineto,
    psMoveto,
    psNewpath,
    psPathbbox,
    psPathforall,
    psRcurveto,
    psRlineto,
    psRmoveto,
  )
import PostScript.Primitives.Graphics.Transform
  ( psConcat,
    psConcatmatrix,
    psCurrentmatrix,
    psDefaultmatrix,
    psDtransform,
    psIdentmatrix,
    psIdtransform,
    psInitmatrix,
    psInvertmatrix,
    psItransform,
    psMatrix,
    psRotate,
    psScale,
    psSetmatrix,
    psTransform,
    psTranslate,
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
  ( psArray,
    psClear,
    psClearToMark,
    psCopy,
    psCount,
    psCountToMark,
    psDup,
    psEndArray,
    psEndDict,
    psExch,
    psForall,
    psGet,
    psIndex,
    psLength,
    psMark,
    psPop,
    psPrint,
    psPrintDebug,
    psPrintEq,
    psPstack,
    psPut,
    psQuit,
    psRoll,
    psStartDict,
  )
import PostScript.Types (Interpreter, Object (..), PSState (..), defaultSystemDict)

evalProgram :: [Object] -> Interpreter ()
evalProgram [] = return ()
evalProgram (x : xs) = do
  term <- gets executionTerminated
  ex <- gets loopExited
  unless (term || ex) $ do
    evalObject x
    term' <- gets executionTerminated
    ex' <- gets loopExited
    unless (term' || ex') (evalProgram xs)

-- Evaluates a single PostScript object according to PostScript semantics
evalObject :: Object -> Interpreter ()
evalObject (PSOperator name) = executeBuiltin name
evalObject (PSSymbol name) = do
  mVal <- lookupDict name
  case mVal of
    Just (PSBlock code) -> evalProgram code
    Just val -> push val
    Nothing -> executeBuiltin name
evalObject obj = push obj

-- Builtin operator dispatcher
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
  -- Container & Array Primitives
  "array" -> psArray
  "length" -> psLength
  "get" -> psGet
  "put" -> psPut
  "forall" -> psForall evalProgram
  "[" -> psMark
  "]" -> psEndArray
  "<<" -> psStartDict
  ">>" -> psEndDict
  "mark" -> psMark
  "counttomark" -> psCountToMark
  "cleartomark" -> psClearToMark
  -- Output & Session Primitives
  "=" -> psPrintEq
  "==" -> psPrintDebug
  "print" -> psPrint
  "quit" -> psQuit
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
  "exec" -> psExec evalProgram
  "loop" -> psLoop evalProgram
  "exit" -> psExit
  "bind" -> psBind
  -- Graphics 2D Matrix & Transformation Primitives
  "matrix" -> psMatrix
  "initmatrix" -> psInitmatrix
  "defaultmatrix" -> psDefaultmatrix
  "identmatrix" -> psIdentmatrix
  "currentmatrix" -> psCurrentmatrix
  "setmatrix" -> psSetmatrix
  "translate" -> psTranslate
  "scale" -> psScale
  "rotate" -> psRotate
  "concat" -> psConcat
  "concatmatrix" -> psConcatmatrix
  "invertmatrix" -> psInvertmatrix
  "transform" -> psTransform
  "itransform" -> psItransform
  "dtransform" -> psDtransform
  "idtransform" -> psIdtransform
  -- Graphics Path Construction & Geometry Primitives 
  "newpath" -> psNewpath
  "moveto" -> psMoveto
  "rmoveto" -> psRmoveto
  "lineto" -> psLineto
  "rlineto" -> psRlineto
  "curveto" -> psCurveto
  "rcurveto" -> psRcurveto
  "arc" -> psArc
  "arcn" -> psArcn
  "arcto" -> psArcto
  "closepath" -> psClosepath
  "currentpoint" -> psCurrentpoint
  "pathbbox" -> psPathbbox
  "flattenpath" -> psFlattenpath
  "pathforall" -> psPathforall evalProgram
  -- TODO: wire container ops: "string", "cvs"
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