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



