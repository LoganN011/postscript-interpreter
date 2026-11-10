module PostScript.Primitives.Dictionary
  ( psDef,
    psDict,
    psBegin,
    psEnd,
    psKnown,
    psLoad,
    psWhere,
    psCleardictstack,
    psUserdict,
    popKey,
    toKey,
  )
where

import Control.Monad.State.Strict (gets, modify)
import Data.Map.Strict qualified as Map
import PostScript.Environment
  ( defineSymbol,
    isBuiltin,
    lookupDict,
    newDict,
    pop,
    popDict,
    push,
    pushDict,
  )
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

-- 'load': key load -> search dict stack for key's value (falling back to builtins)
psLoad :: Interpreter ()
psLoad = do
  key <- popKey
  mVal <- lookupDict key
  case mVal of
    Just val -> push val
    Nothing
      | isBuiltin key -> push (PSOperator key)
      | otherwise -> error ("symbol not found: " ++ key)

-- 'where': key where -> dict true OR false (search dict stack for dict containing key)
psWhere :: Interpreter ()
psWhere = do
  key <- popKey
  dStack <- gets dictStack
  dHeap <- gets dictHeap
  case findDict key dStack dHeap of
    Just did -> push (PSDict did) >> push (PSBoolean True)
    Nothing
      | isBuiltin key -> push (PSDict 0) >> push (PSBoolean True)
      | otherwise -> push (PSBoolean False)
  where
    findDict _ [] _ = Nothing
    findDict k (did : ds) heap =
      case Map.lookup did heap of
        Just d | Map.member k d -> Just did
        _ -> findDict k ds heap

-- 'cleardictstack': pops all dictionaries off dictStack except root
psCleardictstack :: Interpreter ()
psCleardictstack = modify (\s -> s {dictStack = case dictStack s of [] -> [0]; ds -> [last ds]})

-- 'userdict': -> dict (pushes user dictionary onto operand stack)
psUserdict :: Interpreter ()
psUserdict = push (PSDict 0)
