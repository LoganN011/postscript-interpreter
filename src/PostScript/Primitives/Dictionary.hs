module PostScript.Primitives.Dictionary 
(psDef,
psDict,
psBegin,
psEnd,
psKnown,
psLoad)where


import qualified Data.Map.Strict as Map
import PostScript.Environment (defineSymbol, lookupDict, pop, popDict, push, pushDict)
import PostScript.Types

-- 'def': key value def ->
psDef :: Interpreter ()
psDef = do 
   val <- pop
   keyObj <- pop
   case keyObj of 
      PSName key -> defineSymbol key val
      PSSymbol key -> defineSymbol key val
      _ -> error "Type error: key must be name or symbol"

-- 'dict': int dict -> dict
psDict :: Interpreter ()
psDict = do
   capObj <- pop
   case capObj of 
      PSInteger n 
         | n < 0 -> error "dict capacity cannot be negative"
         | otherwise -> push (PSDict Map.empty)
      _ -> error "Type error: argument must be an int"

-- 'begin': dict begin ->
psBegin :: Interpreter ()
psBegin = do
   dictObj <- pop
   case dictObj of 
      PSDict d -> pushDict d
      _ -> error "Type error: argument must be a dict"

-- 'end': end ->
psEnd :: Interpreter ()
psEnd = () <$ popDict

-- 'known': dict key known -> bool
psKnown :: Interpreter ()
psKnown = do 
   keyObj <- pop
   dictObj <- pop
   case (dictObj, keyObj) of 
      (PSDict d, PSName key) -> push (PSBoolean (Map.member key d))
      (PSDict d, PSSymbol key) -> push (PSBoolean (Map.member key d))
      _ -> error "Type error: arguments must be dict and name"

-- 'load': key load -> value
psLoad :: Interpreter ()
psLoad = do
   keyObj <- pop
   case keyObj of 
      PSName key -> do
         mVal <- lookupDict key
         case mVal of 
            Just val -> push val
            Nothing -> error ("symbol not found " ++ key)
      PSSymbol key -> do
         mVal <- lookupDict key
         case mVal of 
            Just val -> push val
            Nothing -> error ("symbol not found " ++ key)
      _ -> error "Type error: arguments must be name or symbol"

