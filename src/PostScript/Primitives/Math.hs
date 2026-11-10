module PostScript.Primitives.Math
  ( psAdd,
    psSub,
    psMul,
    psDiv,
    psIdiv,
    psMod,
    psNeg,
    psAbs,
    psCeiling,
    psFloor,
    psRound,
    psTruncate,
    psCvi,
    psCvr,
    psRand,
    psSrand,
    psRrand,
    psUsertime,
    psSqrt,
    psAtan,
    psCos,
    psSin,
    psExp,
    psLn,
    psLog,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (gets, modify)
import Data.Bits ((.&.))
import PostScript.Environment (pop, push)
import PostScript.Types
import System.CPUTime (getCPUTime)

-- extract numeric values as Double
toDouble :: Object -> Maybe Double
toDouble (PSInteger n) = Just (fromIntegral n)
toDouble (PSReal r) = Just r
toDouble _ = Nothing

-- 'add': num1 num2 add -> sum
psAdd :: Interpreter ()
psAdd = do
  b <- pop
  a <- pop
  case (a, b) of
    (PSInteger x, PSInteger y) -> push (PSInteger (x + y))
    _ -> case (toDouble a, toDouble b) of
      (Just x, Just y) -> push (PSReal (x + y))
      _ -> error "Type error: arguements must be numbers in add"

-- 'sub': num1 num2 sub -> difference (num1 - num2)
psSub :: Interpreter ()
psSub = do
  b <- pop
  a <- pop
  case (a, b) of
    (PSInteger x, PSInteger y) -> push (PSInteger (x - y))
    _ -> case (toDouble a, toDouble b) of
      (Just x, Just y) -> push (PSReal (x - y))
      _ -> error "Type error: arguements must be numbers in sub"

-- 'mul': num1 num2 mul -> product
psMul :: Interpreter ()
psMul = do
  b <- pop
  a <- pop
  case (a, b) of
    (PSInteger x, PSInteger y) -> push (PSInteger (x * y))
    _ -> case (toDouble a, toDouble b) of
      (Just x, Just y) -> push (PSReal (x * y))
      _ -> error "Type error: arguements must be numbers in mul"

--  'div': num1 num2 div -> quotient (always yields a real in PostScript)
psDiv :: Interpreter ()
psDiv = do
  b <- pop
  a <- pop
  case (toDouble a, toDouble b) of
    (Just _, Just 0.0) -> error "Division by zero"
    (Just x, Just y) -> push (PSReal (x / y))
    _ -> error "Type error: arguements must be numbers in div"

-- 'idiv': int1 int2 idiv -> integer quotient
psIdiv :: Interpreter ()
psIdiv = do
  b <- pop
  a <- pop
  case (a, b) of
    (_, PSInteger 0) -> error "Division by zero"
    (PSInteger x, PSInteger y) -> push (PSInteger (x `quot` y))
    _ -> error "Type error: arguements must be numbers in Idiv"

-- 'mod': int1 int2 mod -> remainder
psMod :: Interpreter ()
psMod = do
  b <- pop
  a <- pop
  case (a, b) of
    (_, PSInteger 0) -> error "Undefinedresult: modulo by zero"
    (PSInteger x, PSInteger y) -> push (PSInteger (x `rem` y))
    _ -> error "type error: arguements must be numbers in mod"

-- 'neg': num1 neg -> -num1
psNeg :: Interpreter ()
psNeg = do
  val <- pop
  case val of
    PSInteger n -> push (PSInteger (-n))
    PSReal r -> push (PSReal (-r))
    _ -> error "type error: arguement must be numbers in neg"

-- 'abs': num1 abs -> |num1|
psAbs :: Interpreter ()
psAbs = do
  val <- pop
  case val of
    PSInteger n -> push (PSInteger (abs n))
    PSReal r -> push (PSReal (abs r))
    _ -> error "type error: arguement must be numbers in abs"

-- 'ceiling': num1 ceiling -> least integer >= num1 (as real)
psCeiling :: Interpreter ()
psCeiling = do
  val <- pop
  case toDouble val of
    Just r -> push (PSReal (fromIntegral (ceiling r :: Integer)))
    Nothing -> error "type error: arguement must be numbers in ceiling"

-- 'floor': num1 floor -> greatest integer <= num1 (as real)
psFloor :: Interpreter ()
psFloor = do
  val <- pop
  case toDouble val of
    Just r -> push (PSReal (fromIntegral (floor r :: Integer)))
    Nothing -> error "type error: arguement must be numbers in floor"

-- 'round': num1 round -> nearest integer (as real, greater of two if tied)
psRound :: Interpreter ()
psRound = do
  val <- pop
  case toDouble val of
    Just r -> push (PSReal (fromIntegral (floor (r + 0.5) :: Integer)))
    Nothing -> error "type error: arguement must be numbers in round"

-- 'truncate': num1 truncate -> integer part truncated toward zero (as real)
psTruncate :: Interpreter ()
psTruncate = do
  val <- pop
  case toDouble val of
    Just r -> push (PSReal (fromIntegral (truncate r :: Integer)))
    Nothing -> error "type error: arguement must be numbers in truncate"

-- 'sqrt': num1 sqrt -> square root
psSqrt :: Interpreter ()
psSqrt = do
  val <- pop
  case toDouble val of
    Just r
      | r < 0.0 -> error "Cannot sqrt a negative number"
      | otherwise -> push (PSReal (sqrt r))
    Nothing -> error "type error: arguement must be numbers in sqrt"

-- 'atan': num den atan -> angle in degrees in range [0, 360)
psAtan :: Interpreter ()
psAtan = do
  denObj <- pop
  numObj <- pop
  case (toDouble numObj, toDouble denObj) of
    (Just y, Just x) -> do
      let rad = atan2 y x
          deg = rad * 180.0 / pi
          normalized = if deg < 0 then deg + 360.0 else deg
      push (PSReal normalized)
    _ -> error "type error: arguements must be numbers in atan"

-- 'cos': angle(deg) cos -> cosine
psCos :: Interpreter ()
psCos = do
  val <- pop
  case toDouble val of
    Just deg ->
      let res = cos (deg * pi / 180.0)
          clean = if abs res < 1e-12 then 0.0 else res
       in push (PSReal clean)
    Nothing -> error "type error: arguements must be numbers in Cos"

-- 'sin': angle(deg) sin -> sine
psSin :: Interpreter ()
psSin = do
  val <- pop
  case toDouble val of
    Just deg ->
      let res = sin (deg * pi / 180.0)
          clean = if abs res < 1e-12 then 0.0 else res
       in push (PSReal clean)
    Nothing -> error "Type error: arguements must be numbers in sin"

-- 'exp': base exp -> base^exp
psExp :: Interpreter ()
psExp = do
  expObj <- pop
  baseObj <- pop
  case (toDouble baseObj, toDouble expObj) of
    (Just b, Just e) -> push (PSReal (b ** e))
    _ -> error "Type error: arguements must be numbers in exp"

-- 'ln': num ln -> natural log
psLn :: Interpreter ()
psLn = do
  val <- pop
  case toDouble val of
    Just r
      | r <= 0.0 -> error "Cannot ln a negative number"
      | otherwise -> push (PSReal (log r))
    Nothing -> error "Type error: arguements must be numbers in ln"

-- 'log': num log -> log base 10
psLog :: Interpreter ()
psLog = do
  val <- pop
  case toDouble val of
    Just r
      | r <= 0.0 -> error "cannot log a negative number"
      | otherwise -> push (PSReal (logBase 10.0 r))
    Nothing -> error "Type error: arguements must be numbers in log"

-- 'cvi': num | string cvi -> int (truncate toward zero or parse integer)
psCvi :: Interpreter ()
psCvi =
  pop >>= \case
    PSInteger n -> push (PSInteger n)
    PSReal r -> push (PSInteger (truncate r))
    PSString s -> case reads s :: [(Int, String)] of
      [(n, "")] -> push (PSInteger n)
      _ -> case reads s :: [(Double, String)] of
        [(d, "")] -> push (PSInteger (truncate d))
        _ -> error "typecheck: cvi string parse error"
    _ -> error "typecheck: cvi expected number or string"

-- 'cvr': num | string cvr -> real
psCvr :: Interpreter ()
psCvr =
  pop >>= \case
    PSInteger n -> push (PSReal (fromIntegral n))
    PSReal r -> push (PSReal r)
    PSString s -> case reads s :: [(Double, String)] of
      [(d, "")] -> push (PSReal d)
      _ -> case reads s :: [(Int, String)] of
        [(n, "")] -> push (PSReal (fromIntegral n))
        _ -> error "typecheck: cvr string parse error"
    _ -> error "typecheck: cvr expected number or string"

-- 'rand': -> int (pseudo-random integer in range [0, 2^31-1])
psRand :: Interpreter ()
psRand = do
  s <- gets randSeed
  let next = (s * 1103515245 + 12345) .&. 0x7FFFFFFF
  modify (\st -> st {randSeed = next})
  push (PSInteger next)

-- 'srand': int srand -> (seed pseudo-random generator)
psSrand :: Interpreter ()
psSrand = do
  seed <-
    pop >>= \case
      PSInteger n -> return (n .&. 0x7FFFFFFF)
      _ -> error "typecheck: srand expected integer"
  modify (\st -> st {randSeed = seed})

-- 'rrand': -> int (current random seed)
psRrand :: Interpreter ()
psRrand = do
  seed <- gets randSeed
  push (PSInteger seed)

-- 'usertime': -> int (CPU time in milliseconds)
psUsertime :: Interpreter ()
psUsertime = do
  t <- liftIO getCPUTime
  push (PSInteger (fromIntegral (t `div` 1000000000)))