{- ============================================================================
   MODULE: PostScript.Primitives.Stack
   PATH:   src/PostScript/Primitives/Stack.hs

   DESCRIPTION:
     Implements standard PostScript operand stack manipulation primitives.

   RESPONSIBILITIES:
     1. Core Stack Operators:
        - `pop`: Discard top item from `operandStack`.
        - `dup`: Duplicate top item.
        - `exch`: Exchange top two items on `operandStack`.
        - `index`: Copy $n$-th item from stack top down to top of stack.
        - `roll`: Rotate top $n$ items by $j$ positions.
        - `copy`: Duplicate top $n$ items on stack.
        - `clear`: Clear all items off `operandStack`.
        - `count`: Push current number of items on `operandStack`.
        - `pstack`: Print current stack contents to stdout (debugging helper).

   FUNCTIONS TO IMPLEMENT:
     - psPop :: Interpreter ()
     - psDup :: Interpreter ()
     - psExch :: Interpreter ()
     - psIndex :: Interpreter ()
     - psRoll :: Interpreter ()
     - psCopy :: Interpreter ()
     - psClear :: Interpreter ()
     - psCount :: Interpreter ()
     - psPstack :: Interpreter ()
   ============================================================================ -}

module PostScript.Primitives.Stack 
(psPop, 
psDup, 
psExch, 
psIndex, 
psRoll, 
psCopy, 
psClear, 
psCount,
psPstack) where
  
import Control.Monad.State
import PostScript.Environment (pop, push)
import PostScript.Types

-- 'pop': Discards the top element from the operand stack
psPop :: Interpreter ()
psPop = () <$ pop

-- 'dup': Duplicates the top element on the operand stack
psDup :: Interpreter ()
psDup = do
  x <- pop
  push x 
  push x

-- 'exch': Swaps the top two elements on the operand stack
psExch :: Interpreter ()
psExch = do 
  x <- pop
  y <- pop
  push x
  push y

-- 'index': Retrieves the n-th element down the stack (0-indexed)
psIndex :: Interpreter ()
psIndex = do 
  nObj <- pop
  case nObj of 
    PSInteger n 
      | n < 0 -> error "Index cannot be negative"
      |otherwise -> do
        stack <- gets operandStack
        if length stack > n
          then push (stack !! n)
          else error "Index outofbounds"
    _ -> error "Type error: arguments must be ints"

-- 'roll': Rolls the top n elements by j positions
psRoll :: Interpreter ()
psRoll = do 
  jObj <- pop
  nObj <- pop
  case (nObj, jObj) of
    (PSInteger n,PSInteger j)
      | n < 0 -> error "Cannot Roll negative count"
      | n == 0 || n == 1 -> return ()
      | otherwise -> (do
          stack <- gets operandStack
          if length stack < n
            then error "Stack underflow"
            else do
              let (target, rest) = splitAt n stack
                  shift = (j `mod` n + n) `mod` n
                  (front, back) = splitAt shift target
                  rolled = back ++ front
              modify (\s -> s { operandStack = rolled ++ rest })
      )
    _ -> error "Type error: arguments must be ints"

-- 'copy': Duplicates the top n elements
psCopy :: Interpreter ()
psCopy = do 
  nObj <- pop
  case nObj of 
    PSInteger n
      | n < 0 -> error "Cannot copy a negative amount"
      | otherwise -> do 
        stack <- gets operandStack
        if length stack < n
            then error "Stack underflow"
            else do
              let topN = take n stack
              modify (\s -> s {operandStack = topN ++ stack})
    _ -> error "Type error: argumetns must be ints"

-- 'clear': Removes all elements from the operand stack
psClear :: Interpreter ()
psClear = modify (\s -> s {operandStack = []})

-- 'count': Pushes the total number of items currently on the operand stack
psCount :: Interpreter ()
psCount = do
  n <- gets (length.operandStack)
  push(PSInteger n)

-- 'pstack': Prints the contents of the operand stack to stdout (top item printed first)
psPstack :: Interpreter ()
psPstack = do
  stack <- gets operandStack
  liftIO $ do
    putStrLn "--- top of stack ---"
    mapM_ print stack
    putStrLn "--- bottom of stack ---"



