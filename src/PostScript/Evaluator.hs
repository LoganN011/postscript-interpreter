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
import Control.Monad.State.Strict (execStateT, gets)
import PostScript.Environment (lookupDict, push)
import PostScript.Primitives.Control
  ( psAnd,
    psBind,
    psBitshift,
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
    psCleardictstack,
    psDef,
    psDict,
    psEnd,
    psKnown,
    psLoad,
    psUserdict,
    psWhere,
  )
import PostScript.Primitives.Graphics.Path
  ( psArc,
    psArcn,
    psArcto,
    psClippath,
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
import PostScript.Primitives.Graphics.State
  ( psClip,
    psCurrentgray,
    psCurrentlinewidth,
    psCurrentmiterlimit,
    psCurrentrgbcolor,
    psEofill,
    psFill,
    psGrestore,
    psGrestoreall,
    psGsave,
    psRestore,
    psSave,
    psSetcmykcolor,
    psSetdash,
    psSetgray,
    psSethsbcolor,
    psSetlinecap,
    psSetlinejoin,
    psSetlinewidth,
    psSetmiterlimit,
    psSetrgbcolor,
    psShowpage,
    psStroke,
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
    psCvi,
    psDiv,
    psExp,
    psFloor,
    psIdiv,
    psLn,
    psLog,
    psMod,
    psMul,
    psNeg,
    psRand,
    psRound,
    psRrand,
    psSin,
    psSqrt,
    psSrand,
    psSub,
    psTruncate,
    psUsertime,
  )
import PostScript.Primitives.Stack
  ( psAload,
    psArray,
    psAstore,
    psClear,
    psClearToMark,
    psCopy,
    psCount,
    psCountToMark,
    psCvx,
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
    Just (PSOperator op) -> executeBuiltin op
    Just (PSSymbol sym) -> evalObject (PSSymbol sym)
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
  "aload" -> psAload
  "astore" -> psAstore
  "cvx" -> psCvx
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
  "cvi" -> psCvi
  "rand" -> psRand
  "srand" -> psSrand
  "rrand" -> psRrand
  "bitshift" -> psBitshift
  "usertime" -> psUsertime
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
  "where" -> psWhere
  "cleardictstack" -> psCleardictstack
  "userdict" -> psUserdict
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
  "clippath" -> psClippath
  -- Graphics State, Color & Painting Primitives
  "gsave" -> psGsave
  "grestore" -> psGrestore
  "grestoreall" -> psGrestoreall
  "save" -> psSave
  "restore" -> psRestore
  "setrgbcolor" -> psSetrgbcolor
  "setgray" -> psSetgray
  "setcmykcolor" -> psSetcmykcolor
  "sethsbcolor" -> psSethsbcolor
  "currentrgbcolor" -> psCurrentrgbcolor
  "currentgray" -> psCurrentgray
  "currentlinewidth" -> psCurrentlinewidth
  "setlinewidth" -> psSetlinewidth
  "setlinecap" -> psSetlinecap
  "setlinejoin" -> psSetlinejoin
  "setmiterlimit" -> psSetmiterlimit
  "currentmiterlimit" -> psCurrentmiterlimit
  "setdash" -> psSetdash
  "stroke" -> psStroke
  "fill" -> psFill
  "eofill" -> psEofill
  "clip" -> psClip
  "showpage" -> psShowpage
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