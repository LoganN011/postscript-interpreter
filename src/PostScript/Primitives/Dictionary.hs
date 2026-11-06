module PostScript.Primitives.Dictionary
  ( psDef,
    psDict,
    psBegin,
    psEnd,
    psKnown,
    psLoad,
    popKey,
    toKey,
  )
where

import Control.Monad.State (gets)
import Data.Map.Strict qualified as Map
import PostScript.Environment (defineSymbol, lookupDict, newDict, pop, popDict, push, pushDict)
import PostScript.Types

-- Extract string representation of a key (name, symbol, or string)
toKey :: Object -> String
toKey (PSName k) = k
toKey (PSSymbol k) = k
toKey (PSString k) = k
toKey _ = error "key must be name, symbol, or string"

-- Pop key from the operand stack
popKey :: Interpreter String
popKey = toKey <$> pop

-- 'def': key value def -> bind in active dictionary
psDef :: Interpreter ()
psDef = do
  val <- pop
  key <- popKey
  defineSymbol key val

-- 'dict': int dict -> allocate empty dictionary
psDict :: Interpreter ()
psDict =
  pop >>= \case
    PSInteger n | n >= 0 -> newDict Map.empty >>= push
    _ -> error "dict capacity must be non-negative int"

-- 'begin': dict begin -> push dict onto dictStack
psBegin :: Interpreter ()
psBegin =
  pop >>= \case
    PSDict did -> pushDict did
    _ -> error "begin requires dict"

-- 'end': end -> pop dict off dictStack
psEnd :: Interpreter ()
psEnd = () <$ popDict

-- 'known': dict key known -> test if key exists in dict
psKnown :: Interpreter ()
psKnown = do
  key <- popKey
  pop >>= \case
    PSDict did -> gets (Map.member key . Map.findWithDefault Map.empty did . dictHeap) >>= push . PSBoolean
    _ -> error "known requires dict"

-- 'load': key load -> search dict stack for key's value
psLoad :: Interpreter ()
psLoad = do
  key <- popKey
  mVal <- lookupDict key
  maybe (error ("symbol not found: " ++ key)) push mVal
