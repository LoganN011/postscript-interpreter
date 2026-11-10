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
    psSystemdict,
    psCurrentdict,
    psCountdictstack,
    psDictstack,
    psMaxlength,
    psUndef,
    psStore,
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

-- Extract string representation of a key (name, symbol, string, integer, etc.)
toKey :: Object -> String
toKey (PSName k) = k
toKey (PSSymbol k) = k
toKey (PSString k) = k
toKey (PSInteger n) = show n
toKey (PSReal r) = show r
toKey (PSBoolean b) = show b
toKey other = show other

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
    PSDict did -> do
      m <- gets (Map.findWithDefault Map.empty did . dictHeap)
      let isK = Map.member key m || (did == 0 && isBuiltin key)
      push (PSBoolean isK)
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
  case findDictInStack key dStack dHeap of
    Just did -> push (PSDict did) >> push (PSBoolean True)
    Nothing
      | isBuiltin key -> push (PSDict 0) >> push (PSBoolean True)
      | otherwise -> push (PSBoolean False)

-- Search dictionary stack from top to bottom for dictionary containing key
findDictInStack :: String -> [Int] -> Map.Map Int (Map.Map String Object) -> Maybe Int
findDictInStack _ [] _ = Nothing
findDictInStack k (did : ds) heap =
  case Map.lookup did heap of
    Just d | Map.member k d -> Just did
    _ -> findDictInStack k ds heap

-- 'cleardictstack': pops all dictionaries off dictStack except root standard dicts
psCleardictstack :: Interpreter ()
psCleardictstack = modify (\s -> s {dictStack = [0, 0, 0]})

-- 'userdict': -> dict (pushes user dictionary onto operand stack)
psUserdict :: Interpreter ()
psUserdict = push (PSDict 0)

-- 'systemdict': -> dict (pushes system dictionary onto operand stack)
psSystemdict :: Interpreter ()
psSystemdict = push (PSDict 0)

-- 'currentdict': -> dict (pushes current active dictionary onto operand stack)
psCurrentdict :: Interpreter ()
psCurrentdict = do
  dids <- gets dictStack
  case dids of
    (did : _) -> push (PSDict did)
    [] -> push (PSDict 0)

-- 'countdictstack': -> int (pushes number of dictionaries on dictStack)
psCountdictstack :: Interpreter ()
psCountdictstack = gets (length . dictStack) >>= push . PSInteger

-- 'dictstack': array dictstack -> subarray (copies dictStack to array)
psDictstack :: Interpreter ()
psDictstack = do
  obj <- pop
  case obj of
    PSArray aid -> do
      dids <- gets (reverse . dictStack)
      let dictObjs = map PSDict dids
      modify (\s -> s {arrayHeap = Map.insert aid dictObjs (arrayHeap s)})
      push (PSArray aid)
    _ -> error "dictstack requires array"

-- 'maxlength': dict maxlength -> int
psMaxlength :: Interpreter ()
psMaxlength = do
  pop >>= \case
    PSDict did -> do
      heap <- gets dictHeap
      case Map.lookup did heap of
        Just d -> push (PSInteger (max 10 (Map.size d)))
        Nothing -> push (PSInteger 10)
    _ -> error "maxlength requires dict"

-- 'undef': dict key undef -> remove key from dict
psUndef :: Interpreter ()
psUndef = do
  key <- popKey
  pop >>= \case
    PSDict did -> modify (\s -> s {dictHeap = Map.adjust (Map.delete key) did (dictHeap s)})
    _ -> error "undef requires dict"

-- 'store': key value store -> replace value in dict containing key, or put in currentdict
psStore :: Interpreter ()
psStore = do
  val <- pop
  key <- popKey
  dStack <- gets dictStack
  dHeap <- gets dictHeap
  case findDictInStack key dStack dHeap of
    Just did -> modify (\s -> s {dictHeap = Map.adjust (Map.insert key val) did (dictHeap s)})
    Nothing -> defineSymbol key val
