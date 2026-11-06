module PostScript.Environment
  ( push,
    pop,
    peek,
    popTwo,
    lookupDict,
    defineSymbol,
    pushDict,
    popDict,
    newArray,
    newDict,
    getGState,
    modifyGState,
  )
where

import Control.Monad.State (gets, modify)
import Data.Foldable (asum)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import PostScript.Types

-- Push Object onto the operand stack
push :: Object -> Interpreter ()
push obj = modify (\s -> s {operandStack = obj : operandStack s})

-- Pop Object off the operand stack
pop :: Interpreter Object
pop = do
  st <- gets operandStack
  case st of
    (x : xs) -> modify (\s -> s {operandStack = xs}) >> return x
    [] -> error "stack empty"

-- Look at the top Object on the operand stack
peek :: Interpreter Object
peek =
  gets operandStack >>= \case
    (x : _) -> return x
    [] -> error "stack empty on peek"

-- Pop top two items of the operand stack: returns (top, second)
popTwo :: Interpreter (Object, Object)
popTwo = do
  top    <- pop
  second <- pop
  return (top, second)

-- Search the dictionary stack from top to bottom
lookupDict :: String -> Interpreter (Maybe Object)
lookupDict key = gets $ \s ->
  asum [Map.lookup key d | did <- dictStack s, Just d <- [Map.lookup did (dictHeap s)]]

-- Bind a key-value pair in the active (top) dictionary
defineSymbol :: String -> Object -> Interpreter ()
defineSymbol key val = modify $ \s -> case dictStack s of
  [] -> error "no active dictionary"
  (did : _) -> s {dictHeap = Map.adjust (Map.insert key val) did (dictHeap s)}

-- Push a dictionary onto the dictionary stack
pushDict :: Int -> Interpreter ()
pushDict did = modify (\s -> s {dictStack = did : dictStack s})

-- Pop a dictionary off the dictionary stack (cannot pop root system dict)
popDict :: Interpreter Int
popDict = do
  dids <- gets dictStack
  case dids of
    (did : ds@(_ : _)) -> modify (\s -> s {dictStack = ds}) >> return did
    _ -> error "Cannot pop root dictionary"

-- Allocate a new array on the heap
newArray :: [Object] -> Interpreter Object
newArray xs = do
  aid <- gets nextId
  modify (\s -> s {arrayHeap = Map.insert aid xs (arrayHeap s), nextId = aid + 1})
  return (PSArray aid)

-- Allocate a new dictionary on the heap
newDict :: Map String Object -> Interpreter Object
newDict d = do
  did <- gets nextId
  modify (\s -> s {dictHeap = Map.insert did d (dictHeap s), nextId = did + 1})
  return (PSDict did)

-- Get the active GraphicsState
getGState :: Interpreter GraphicsState
getGState = gets gState

-- Change the active GraphicsState
modifyGState :: (GraphicsState -> GraphicsState) -> Interpreter ()
modifyGState f = modify (\s -> s {gState = f (gState s)})
