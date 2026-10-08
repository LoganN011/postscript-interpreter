{- ============================================================================
   MODULE: PostScript.Primitives.Control
   PATH:   src/PostScript/Primitives/Control.hs

   DESCRIPTION:
     Implements relational comparisons, relational boolean logic, conditional branches,
     and loop primitives.

   RESPONSIBILITIES:
     1. Relational & Boolean Primitives:
        - `eq`, `ne`, `gt`, `ge`, `lt`, `le`: Compare top two objects, push `PSBoolean`.
        - `and`, `or`, `not`, `xor`: Bitwise/boolean operators.
     2. Conditionals:
        - `if`: Pop `PSBlock` and `PSBoolean`. Execute block if `True`.
        - `ifelse`: Pop block2, block1, boolean. Execute block1 if `True`, block2 if `False`.
     3. Iteration & Loops:
        - `repeat`: Pop `PSBlock` and count $n$. Execute block $n$ times.
        - `for`: Pop `PSBlock`, limit, step, initial. Loop from initial to limit by step,
                 pushing current counter value onto stack each iteration before executing block.
        - `loop`, `exit`: Continuous looping and loop breaking mechanism.

   FUNCTIONS TO IMPLEMENT:
     - psEq, psNe, psGt, psGe, psLt, psLe :: Interpreter ()
     - psAnd, psOr, psNot, psXor :: Interpreter ()
     - psIf, psIfelse :: Interpreter ()
     - psRepeat, psFor :: Interpreter ()
     - psLoop, psExit :: Interpreter ()
   ============================================================================ -}

module PostScript.Primitives.Control where

import Control.Monad (replicateM_, when)
import PostScript.Environment (pop, push)
import PostScript.Types

--extract numeric values as Double
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
      (PSInteger x,PSInteger y) -> push (PSBoolean (x == y))
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
      (PSInteger x,PSInteger y) -> push (PSBoolean (x /= y))
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
         _ -> error ("Typer error: " ++ opName ++" arguments must be numbers")


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

-- 'and': bool1 bool2 and -> bool
psAnd :: Interpreter ()
psAnd = do 
   b <- pop
   a <- pop
   case (a, b) of
      (PSBoolean x, PSBoolean y) -> push (PSBoolean (x && y))
      _ -> error "Type error: and arguments must be bools"

-- 'or': bool1 bool2 or -> bool
psOr :: Interpreter ()
psOr = do 
   b <- pop
   a <- pop
   case (a, b) of
      (PSBoolean x, PSBoolean y) -> push (PSBoolean (x || y))
      _ -> error "Type error: or arguments must be bools"

-- 'not': bool not -> bool
psNot :: Interpreter ()
psNot = do 
   val <- pop
   case val of 
      PSBoolean b -> push (PSBoolean (not b))
      _ -> error "Type error: not argument must be bool"

-- 'xor': bool1 bool2 xor -> bool
psXor :: Interpreter ()
psXor = do
   b <- pop
   a <- pop
   case (a, b) of
      (PSBoolean x, PSBoolean y) -> push (PSBoolean ((x || y) && not (x && y)))
      _ -> error "Type error: xor arguments must be bools"

-- 'if': bool proc if ->
psIf :: ([Object] -> Interpreter ()) -> Interpreter ()
psIf evalProg = do
   procObj <- pop 
   condObj <- pop
   case (condObj, procObj) of 
      (PSBoolean cond, PSBlock code) ->
         when cond (evalProg code)
      _ -> error "Type error: if expected boolean and procdure block"

-- 'ifelse': bool proc1 proc2 ifelse ->
psIfelse :: ([Object] -> Interpreter ()) -> Interpreter ()
psIfelse evalProg = do
   proc2Obj <- pop
   proc1Obj <- pop
   condObj <- pop
   case (condObj, proc1Obj, proc2Obj) of
      (PSBoolean cond, PSBlock code1,PSBlock code2) ->
         if cond then evalProg code1 else evalProg code2
      _ -> error "Type error: ifelse expected bolean and 2 produre blocks"

-- 'repeat': int proc repeat ->
psRepeat :: ([Object] -> Interpreter ()) -> Interpreter ()
psRepeat evalProg = do 
   procObj <- pop
   countObj <- pop
   case (countObj, procObj) of 
      (PSInteger n, PSBlock code)
         | n < 0 -> error "negative iteration count in repeat"
         | otherwise -> replicateM_ n (evalProg code)
      _ -> error "Type error: repeat expected int and block"

-- 'for': init step limit proc for ->
psFor :: ([Object] -> Interpreter ()) -> Interpreter ()
psFor evalProg = do
   procObj <- pop
   limitObj <- pop
   stepObj <- pop
   initObj <- pop
   case (initObj, stepObj, limitObj, procObj) of
      (PSInteger i, PSInteger s, PSInteger lim, PSBlock code) ->
         loopInt i s lim code 
      _ -> case (toDouble initObj, toDouble stepObj, toDouble limitObj, procObj) of
            (Just i, Just s, Just lim, PSBlock code) ->
               loopReal i s lim code
            _ -> error "Type error: for expected number and block"
      where
         loopInt curr step limit code
            | (step > 0 && curr <= limit) || (step < 0 && curr >= limit) = do
               push (PSInteger curr)
               evalProg code
               loopInt (curr+step) step limit code
            | otherwise = return ()
         
         loopReal curr step limit code 
            | (step > 0 && curr <= limit) || (step < 0 && curr >= limit) = do
               push (PSReal curr)
               evalProg code
               loopReal (curr + step) step limit code
            | otherwise = return ()
