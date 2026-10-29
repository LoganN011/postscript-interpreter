module PostScript.Environment 
  (push,
  pop,
  peek,
  popTwo,
  lookupDict,
  defineSymbol,
  pushDict,
  popDict,
  getGState,
  modifyGState) where

import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import PostScript.Types

--Push Object onto the operand stack
push :: Object -> Interpreter ()
push obj = modify (\s -> s {operandStack = obj : operandStack s})

--Pop Object off the operand stack
--Error if stack empty
pop :: Interpreter Object
pop = do 
  s <- get
  case operandStack s of 
    [] -> error "stack empty"
    (x:xs) -> do 
      put (s {operandStack = xs})
      return x

--Look at the top Object on the operand stack
--error if stack empty
peek :: Interpreter Object
peek = do
  stack <- gets operandStack
  case stack of 
    [] -> error "stack empty on peek"
    (x:_) -> return x

--Pop top two items of the operand stack
-- will be (first,second)
popTwo :: Interpreter (Object, Object)
popTwo = do
  top    <- pop
  second <- pop
  return (top, second)

--Search the dictionary stack
lookupDict :: String -> Interpreter (Maybe Object)
lookupDict key = do
  dicts <- gets dictStack
  return (foldr (\d acc -> case Map.lookup key d of 
                            Just v -> Just v
                            Nothing -> acc) Nothing (reverse dicts))

--Bind a key-value pair in the current dictionary
defineSymbol :: String -> Object -> Interpreter ()
defineSymbol key val = modify (\s ->
  case dictStack s of 
    [] -> error "no active dictionary"
    (d:ds) -> s {dictStack = Map.insert key val d:ds})

--Push a dictionary onto the dictionary stack
pushDict :: Map String Object -> Interpreter ()
pushDict d = modify (\s -> s {dictStack = d : dictStack s})

--Pop a dictionary off the dictionary stack
popDict :: Interpreter (Map String Object)
popDict = do 
  s <- get
  case dictStack s of 
    [] -> error "Dictionary stack empty"
    [_] -> error "Cannot pop root system dictionary"
    (d:ds) -> do
      put (s {dictStack = ds})
      return d

--Get the active GraphicsState
getGState :: Interpreter GraphicsState
getGState = gets gState

--Change the active GraphicsState
modifyGState :: (GraphicsState -> GraphicsState) -> Interpreter ()
modifyGState f = modify (\s -> s { gState = f (gState s) })
