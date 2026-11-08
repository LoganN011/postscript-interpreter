module PostScript.Primitives.Control where

import Control.Monad.State (modify)
import Data.Bits (complement, xor, (.&.), (.|.))
import PostScript.Environment (lookupDict, loopWhile, pop, push, toCode)
import PostScript.Types

-- extract numeric values as Double
toDouble :: Object -> Maybe Double
toDouble (PSInteger n) = Just (fromIntegral n)
toDouble (PSReal r) = Just r
toDouble _ = Nothing

-- 'eq': any1 any2 eq -> bool
psEq :: Interpreter ()
psEq = do
  b <- pop
  a <- pop
  case (a, b) of
    (PSInteger x, PSInteger y) -> push (PSBoolean (x == y))
    (PSReal x, PSReal y) -> push (PSBoolean (x == y))
    _ -> case (toDouble a, toDouble b) of
      (Just x, Just y) -> push (PSBoolean (x == y))
      _ -> push (PSBoolean (a == b))

-- 'ne': any1 any2 ne -> bool
psNe :: Interpreter ()
psNe = do
  b <- pop
  a <- pop
  case (a, b) of
    (PSInteger x, PSInteger y) -> push (PSBoolean (x /= y))
    (PSReal x, PSReal y) -> push (PSBoolean (x /= y))
    _ -> case (toDouble a, toDouble b) of
      (Just x, Just y) -> push (PSBoolean (x /= y))
      _ -> push (PSBoolean (a /= b))

-- Order comparison function
compareNumbers :: (Double -> Double -> Bool) -> (Int -> Int -> Bool) -> String -> Interpreter ()
compareNumbers opReal opInt opName = do
  b <- pop
  a <- pop
  case (a, b) of
    (PSInteger x, PSInteger y) -> push (PSBoolean (opInt x y))
    _ -> case (toDouble a, toDouble b) of
      (Just x, Just y) -> push (PSBoolean (opReal x y))
      _ -> error ("Typer error: " ++ opName ++ " arguments must be numbers")

-- 'gt': num1 num2 gt -> bool
psGt :: Interpreter ()
psGt = compareNumbers (>) (>) "gt"

-- 'ge': num1 num2 ge -> bool
psGe :: Interpreter ()
psGe = compareNumbers (>=) (>=) "ge"

-- 'lt': num1 num2 lt -> bool
psLt :: Interpreter ()
psLt = compareNumbers (<) (<) "lt"

-- 'le': num1 num2 le -> bool
psLe :: Interpreter ()
psLe = compareNumbers (<=) (<=) "le"

-- 'and': bool1 bool2 and -> bool | int1 int2 and -> int
psAnd :: Interpreter ()
psAnd = do
  b <- pop
  a <- pop
  case (a, b) of
    (PSBoolean x, PSBoolean y) -> push (PSBoolean (x && y))
    (PSInteger x, PSInteger y) -> push (PSInteger (x .&. y))
    _ -> error "Type error: and arguments must be bools or ints"

-- 'or': bool1 bool2 or -> bool | int1 int2 or -> int
psOr :: Interpreter ()
psOr = do
  b <- pop
  a <- pop
  case (a, b) of
    (PSBoolean x, PSBoolean y) -> push (PSBoolean (x || y))
    (PSInteger x, PSInteger y) -> push (PSInteger (x .|. y))
    _ -> error "Type error: or arguments must be bools or ints"

-- 'not': bool not -> bool | int not -> int
psNot :: Interpreter ()
psNot = do
  val <- pop
  case val of
    PSBoolean b -> push (PSBoolean (not b))
    PSInteger n -> push (PSInteger (complement n))
    _ -> error "Type error: not argument must be bool or int"

-- 'xor': bool1 bool2 xor -> bool | int1 int2 xor -> int
psXor :: Interpreter ()
psXor = do
  b <- pop
  a <- pop
  case (a, b) of
    (PSBoolean x, PSBoolean y) -> push (PSBoolean (x /= y))
    (PSInteger x, PSInteger y) -> push (PSInteger (x `xor` y))
    _ -> error "Type error: xor arguments must be bools or ints"

-- 'if': bool proc if ->
psIf :: ([Object] -> Interpreter ()) -> Interpreter ()
psIf evalProg = do
  code <- toCode <$> pop
  pop >>= \case
    PSBoolean True -> evalProg code
    PSBoolean False -> return ()
    _ -> error "Type error: if expected boolean"

-- 'ifelse': bool proc1 proc2 ifelse ->
psIfelse :: ([Object] -> Interpreter ()) -> Interpreter ()
psIfelse evalProg = do
  code2 <- toCode <$> pop
  code1 <- toCode <$> pop
  pop >>= \case
    PSBoolean cond -> evalProg (if cond then code1 else code2)
    _ -> error "Type error: ifelse expected boolean"

-- 'repeat': int proc repeat ->
psRepeat :: ([Object] -> Interpreter ()) -> Interpreter ()
psRepeat evalProg = do
  code <- toCode <$> pop
  pop >>= \case
    PSInteger n | n >= 0 -> loopWhile (replicate n (evalProg code))
    _ -> error "Type error: repeat expected non-negative int and proc"

-- 'for': init step limit proc for ->
psFor :: ([Object] -> Interpreter ()) -> Interpreter ()
psFor evalProg = do
  code <- toCode <$> pop
  lim <- pop
  step <- pop
  initObj <- pop
  case (initObj, step, lim) of
    (PSInteger i, PSInteger s, PSInteger l) ->
      loopWhile [push (PSInteger n) >> evalProg code | n <- forRange i s l]
    _ -> case (toDouble initObj, toDouble step, toDouble lim) of
      (Just i, Just s, Just l) ->
        loopWhile [push (PSReal n) >> evalProg code | n <- forRange i s l]
      _ -> error "Type error: for expected numbers and proc"
  where
    forRange curr s l
      | (s > 0 && curr <= l) || (s < 0 && curr >= l) = curr : forRange (curr + s) s l
      | otherwise = []

-- 'loop': proc loop -> repeatedly executes proc until exit
psLoop :: ([Object] -> Interpreter ()) -> Interpreter ()
psLoop evalProg = do
  code <- toCode <$> pop
  loopWhile (repeat (evalProg code))

-- 'exit': exit -> breaks out of innermost enclosing loop
psExit :: Interpreter ()
psExit = modify (\s -> s {loopExited = True})

-- 'exec': any exec -> executes block, symbol, operator, or leaves literal on stack
psExec :: ([Object] -> Interpreter ()) -> Interpreter ()
psExec evalProg = do
  obj <- pop
  case obj of
    PSBlock objs -> evalProg objs
    PSSymbol _ -> evalProg [obj]
    PSOperator _ -> evalProg [obj]
    other -> push other

-- 'bind': proc bind -> proc (inlines builtin operators recursively)
psBind :: Interpreter ()
psBind = do
  obj <- pop
  case obj of
    PSBlock objs -> bindObj (PSBlock objs) >>= push
    other -> push other

bindObj :: Object -> Interpreter Object
bindObj (PSBlock objs) = PSBlock <$> mapM bindObj objs
bindObj (PSSymbol name) = do
  mVal <- lookupDict name
  case mVal of
    Just (PSOperator op) -> pure (PSOperator op)
    Just _ -> pure (PSSymbol name)
    Nothing
      | isBuiltin name -> pure (PSOperator name)
      | otherwise -> pure (PSSymbol name)
bindObj other = pure other

isBuiltin :: String -> Bool
isBuiltin s =
  s
    `elem` words
      "pop dup exch index roll copy clear count pstack array length get put forall [ ] << >> mark counttomark cleartomark = == print quit \
      \add sub mul div idiv mod neg abs ceiling floor round truncate sqrt atan cos sin exp ln log \
      \def dict begin end known load eq ne gt ge lt le and or not xor if ifelse repeat for exec loop exit bind \
      \matrix initmatrix defaultmatrix identmatrix currentmatrix setmatrix translate scale rotate concat concatmatrix invertmatrix transform itransform dtransform idtransform \
      \newpath moveto rmoveto lineto rlineto curveto rcurveto arc arcn arcto closepath currentpoint pathbbox flattenpath pathforall"
