{- ============================================================================
   MODULE: PostScript.Primitives.Math
   PATH:   src/PostScript/Primitives/Math.hs

   DESCRIPTION:
     Implements PostScript mathematical, arithmetic, and trigonometric primitives.

   RESPONSIBILITIES:
     1. Basic Arithmetic Operators:
        - `add`: Pop two numbers, push sum (supports both Int and Real promotion).
        - `sub`: Pop two numbers (b, a), push difference (a - b).
        - `mul`: Pop two numbers, push product.
        - `div`: Pop two numbers (b, a), push real division result (a / b).
        - `idiv`: Integer division (a `div` b).
        - `mod`: Integer modulo (a `mod` b).
     2. Numeric Manipulation:
        - `neg`: Negate top number.
        - `abs`: Absolute value of top number.
        - `ceiling`, `floor`, `round`, `truncate`.
     3. Trigonometric & Exponential Operators:
        - `sqrt`: Square root.
        - `atan`: Arctangent (atan2 y x).
        - `cos`, `sin`: Cosine and sine (input in degrees).
        - `exp`: Exponentiation ($base^{exponent}$).
        - `ln`, `log`: Natural and base-10 logarithms.

   FUNCTIONS TO IMPLEMENT:
     - psAdd, psSub, psMul, psDiv, psIdiv, psMod :: Interpreter ()
     - psNeg, psAbs, psCeil, psFloor, psRound, psTrunc :: Interpreter ()
     - psSqrt, psAtan, psCos, psSin, psExp, psLn, psLog :: Interpreter ()
   ============================================================================ -}

module PostScript.Primitives.Math
(psAdd, 
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
psSqrt, 
psAtan, 
psCos, 
psSin,
psExp, 
psLn,
psLog) where

import PostScript.Environment (pop, push)
import PostScript.Types

--extract numeric values as Double
toDouble :: Object -> Maybe Double
toDouble (PSInteger n) = Just (fromIntegral n)
toDouble (PSReal r) = Just r
toDouble _ = Nothing

-- 'add': num1 num2 add -> sum
psAdd :: Interpreter ()
psAdd = do
   b <- pop
   a <- pop
   case (a,b) of 
      (PSInteger x,PSInteger y) -> push (PSInteger (x + y))
      _ -> case (toDouble a, toDouble b) of
         (Just x,Just y) -> push(PSReal (x + y))
         _ -> error "Type error: arguements must be numbers in add"

-- 'sub': num1 num2 sub -> difference (num1 - num2)
psSub :: Interpreter ()
psSub = do
   b <- pop
   a <- pop
   case (a,b) of 
      (PSInteger x,PSInteger y) -> push (PSInteger (x - y))
      _ -> case (toDouble a, toDouble b) of
         (Just x,Just y) -> push(PSReal (x - y))
         _ -> error "Type error: arguements must be numbers in sub"

-- 'mul': num1 num2 mul -> product
psMul :: Interpreter ()
psMul = do
   b <- pop
   a <- pop
   case (a,b) of 
      (PSInteger x,PSInteger y) -> push (PSInteger (x * y))
      _ -> case (toDouble a, toDouble b) of
         (Just x,Just y) -> push(PSReal (x * y))
         _ -> error "Type error: arguements must be numbers in mul"

--  'div': num1 num2 div -> quotient (always yields a real in PostScript)
psDiv :: Interpreter () 
psDiv = do
   b <- pop
   a <- pop
   case (toDouble a,toDouble b) of 
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
    (_, PSInteger 0)-> error "Undefinedresult: modulo by zero"
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
    Just r  -> push (PSReal (fromIntegral (ceiling r :: Integer)))
    Nothing -> error "type error: arguement must be numbers in ceiling"

-- 'floor': num1 floor -> greatest integer <= num1 (as real)
psFloor :: Interpreter ()
psFloor = do
  val <- pop
  case toDouble val of
    Just r -> push (PSReal (fromIntegral (floor r :: Integer)))
    Nothing -> error "type error: arguement must be numbers in floor"

-- 'round': num1 round -> nearest integer (as real)
psRound :: Interpreter ()
psRound = do
  val <- pop
  case toDouble val of
    Just r -> push (PSReal (fromIntegral (round r :: Integer)))
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
    Just deg -> push (PSReal (cos (deg * pi / 180.0)))
    Nothing -> error "type error: arguements must be numbers in Cos"

-- 'sin': angle(deg) sin -> sine
psSin :: Interpreter ()
psSin = do
  val <- pop
  case toDouble val of
    Just deg -> push (PSReal (sin (deg * pi / 180.0)))
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