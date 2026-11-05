module PostScript.Primitives.Stack 
(psPop, 
psDup, 
psExch, 
psIndex, 
psRoll, 
psCopy, 
psClear, 
psCount,
psPstack,
psPrintEq,
psPrintDebug,
psPrint,
psQuit,
formatObject) where
  
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

-- Helper to output a string using the configured printAction
outputStr :: String -> Interpreter ()
outputStr str = do
  act <- gets printAction
  liftIO (act str)

-- 'pstack': Writes the operand stack to stdout (top element first) using '==' format
psPstack :: Interpreter ()
psPstack = do
  stack <- gets operandStack
  mapM_ (\obj -> outputStr (formatObject True obj ++ "\n")) stack

-- Helper to format an Object as a string.
-- When isDebug is True (for '=='), strings are enclosed in parentheses.
formatObject :: Bool -> Object -> String
formatObject isDebug obj = case obj of
  PSInteger n  -> show n
  PSReal r     -> show r
  PSBoolean b  -> if b then "true" else "false"
  PSString s   -> if isDebug then "(" ++ s ++ ")" else s
  PSName s     -> "/" ++ s
  PSSymbol s   -> s
  PSBlock objs -> "{" ++ unwords (map (formatObject isDebug) objs) ++ "}"
  PSDict _     -> "-dict-"

-- '=': Pop top object and print its value with a newline
psPrintEq :: Interpreter ()
psPrintEq = do
  obj <- pop
  outputStr (formatObject False obj ++ "\n")

-- '==': Pop top object and debug-print syntax representation with newline
psPrintDebug :: Interpreter ()
psPrintDebug = do
  obj <- pop
  outputStr (formatObject True obj ++ "\n")

-- 'print': Pop string/value and print without a trailing newline
psPrint :: Interpreter ()
psPrint = do
  obj <- pop
  case obj of
    PSString s -> outputStr s
    _          -> outputStr (formatObject False obj)

-- 'quit': Terminates interpreter execution cleanly
psQuit :: Interpreter ()
psQuit = modify (\s -> s { executionTerminated = True })

-- TODO: Array & container operators:
-- 'array': int array -> allocate array of size n
-- 'length': array|dict|string length -> number of elements
-- 'get': array|dict|string index|key get -> element at index/key
-- 'put': array|dict|string index|key val put -> update element at index/key
-- 'forall': array|dict proc forall -> iterate elements with proc

-- TODO: String & typography helpers:
-- 'string': int string -> allocate string of size n
-- 'cvs': any string cvs -> convert value to string in buffer



