module PostScript.Primitives.Stack
  ( psPop,
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
    psArray,
    psLength,
    psGet,
    psPut,
    psForall,
    psMark,
    psCountToMark,
    psClearToMark,
    psEndArray,
    psStartDict,
    psEndDict,
    formatObject,
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets, modify)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import PostScript.Environment (newArray, newDict, peek, pop, popTwo, push)
import PostScript.Primitives.Dictionary (toKey)
import PostScript.Types

-- Stack manipulation
psPop :: Interpreter ()
psPop = () <$ pop

psDup :: Interpreter ()
psDup = peek >>= push

psExch :: Interpreter ()
psExch = modify $ \s -> case operandStack s of
  (a : b : rest) -> s {operandStack = b : a : rest}
  _ -> error "stack underflow on exch"

psIndex :: Interpreter ()
psIndex =
  pop >>= \case
    PSInteger n
      | n >= 0 ->
          gets operandStack >>= \st ->
            if length st > n then push (st !! n) else error "index out of bounds"
    _ -> error "index requires non-negative int"

psRoll :: Interpreter ()
psRoll =
  popTwo >>= \case
    (PSInteger j, PSInteger n)
      | n > 1 -> modify $ \s ->
          let (topN, rest) = splitAt n (operandStack s)
              k = (j `mod` n + n) `mod` n
           in s {operandStack = drop k topN ++ take k topN ++ rest}
    (PSInteger _, PSInteger _) -> return ()
    _ -> error "roll requires ints"

psClear :: Interpreter ()
psClear = modify (\s -> s {operandStack = []})

psCount :: Interpreter ()
psCount = gets (length . operandStack) >>= push . PSInteger

psQuit :: Interpreter ()
psQuit = modify (\s -> s {executionTerminated = True})

-- Polymorphic copy (stack duplication, array copying, or dict merging)
psCopy :: Interpreter ()
psCopy =
  pop >>= \case
    PSInteger n -> modify (\s -> s {operandStack = take n (operandStack s) ++ operandStack s})
    PSArray dst ->
      pop >>= \case
        PSArray src -> do
          xs <- gets (Map.findWithDefault [] src . arrayHeap)
          modify (\s -> s {arrayHeap = Map.adjust (\d -> xs ++ drop (length xs) d) dst (arrayHeap s)})
          push (PSArray dst)
        _ -> error "copy type mismatch"
    PSDict dst ->
      pop >>= \case
        PSDict src -> do
          d <- gets (Map.findWithDefault Map.empty src . dictHeap)
          modify (\s -> s {dictHeap = Map.adjust (Map.union d) dst (dictHeap s)})
          push (PSDict dst)
        _ -> error "copy type mismatch"
    _ -> error "copy requires int, array, or dict"

-- Container primitives
psArray :: Interpreter ()
psArray =
  pop >>= \case
    PSInteger n | n >= 0 -> newArray (replicate n PSNull) >>= push
    _ -> error "array size must be non-negative int"

psLength :: Interpreter ()
psLength =
  pop >>= \case
    PSArray aid -> gets (Map.findWithDefault [] aid . arrayHeap) >>= push . PSInteger . length
    PSDict did -> gets (Map.findWithDefault Map.empty did . dictHeap) >>= push . PSInteger . Map.size
    PSString s -> push (PSInteger (length s))
    _ -> error "length requires array, dict, or string"

psGet :: Interpreter ()
psGet = do
  k <- pop
  c <- pop
  case (c, k) of
    (PSArray aid, PSInteger i) -> do
      xs <- gets (Map.findWithDefault [] aid . arrayHeap)
      if i >= 0 && i < length xs then push (xs !! i) else error "array index out of bounds"
    (PSDict did, _) -> do
      d <- gets (Map.findWithDefault Map.empty did . dictHeap)
      maybe (error ("key not found: " ++ toKey k)) push (Map.lookup (toKey k) d)
    (PSString s, PSInteger i) ->
      if i >= 0 && i < length s then push (PSInteger (fromEnum (s !! i))) else error "string index out of bounds"
    _ -> error "invalid arguments to get"

psPut :: Interpreter ()
psPut = do
  val <- pop
  k <- pop
  c <- pop
  case (c, k) of
    (PSArray aid, PSInteger i) ->
      modify (\s -> s {arrayHeap = Map.adjust (\xs -> take i xs ++ [val] ++ drop (i + 1) xs) aid (arrayHeap s)})
    (PSDict did, _) ->
      modify (\s -> s {dictHeap = Map.adjust (Map.insert (toKey k) val) did (dictHeap s)})
    _ -> error "invalid arguments to put"

psForall :: ([Object] -> Interpreter ()) -> Interpreter ()
psForall eval = do
  proc <- pop
  let runProc = eval (case proc of PSBlock objs -> objs; o -> [o])
      execItem = gets executionTerminated >>= \term -> unless term runProc
  pop >>= \case
    PSArray aid -> gets (Map.findWithDefault [] aid . arrayHeap) >>= mapM_ (\x -> push x >> execItem)
    PSDict did -> gets (Map.findWithDefault Map.empty did . dictHeap) >>= mapM_ (\(k, v) -> push (PSName k) >> push v >> execItem) . Map.toList
    _ -> error "forall requires array or dict"

-- Delimiters and Marks: [, ], <<, >>, counttomark, cleartomark
psMark :: Interpreter ()
psMark = push PSMark

psCountToMark :: Interpreter ()
psCountToMark = do
  st <- gets operandStack
  case break (== PSMark) st of
    (items, PSMark : _) -> push (PSInteger (length items))
    _                   -> error "unmatched mark on counttomark"

psClearToMark :: Interpreter ()
psClearToMark = () <$ popToMark

psStartDict :: Interpreter ()
psStartDict = psMark

psEndArray :: Interpreter ()
psEndArray = popToMark >>= newArray >>= push

psEndDict :: Interpreter ()
psEndDict = do
  items <- popToMark
  let pairs = [(toKey k, v) | (k, v) <- chunk2 items]
  newDict (Map.fromList pairs) >>= push
  where
    chunk2 (k : v : rest) = (k, v) : chunk2 rest
    chunk2 [] = []
    chunk2 [_] = error "dict literal requires even number of items"

popToMark :: Interpreter [Object]
popToMark = do
  st <- gets operandStack
  let (items, rest) = break (== PSMark) st
  case rest of
    (PSMark : remaining) -> modify (\s -> s {operandStack = remaining}) >> return (reverse items)
    _ -> error "unmatched mark"

-- Output and formatting
outputStr :: String -> Interpreter ()
outputStr str = gets printAction >>= \act -> liftIO (act str)

psPstack :: Interpreter ()
psPstack = do
  st <- gets operandStack
  h <- gets arrayHeap
  mapM_ (\o -> outputStr (formatObject h True o ++ "\n")) st

psPrintEq :: Interpreter ()
psPrintEq = do
  o <- pop
  h <- gets arrayHeap
  outputStr (formatObject h False o ++ "\n")

psPrintDebug :: Interpreter ()
psPrintDebug = do
  o <- pop
  h <- gets arrayHeap
  outputStr (formatObject h True o ++ "\n")

psPrint :: Interpreter ()
psPrint =
  pop >>= \case
    PSString s -> outputStr s
    o -> gets arrayHeap >>= \h -> outputStr (formatObject h False o)

formatObject :: Map Int [Object] -> Bool -> Object -> String
formatObject _ False (PSString s) = s
formatObject _ False (PSName s) = s
formatObject _ False (PSSymbol s) = s
formatObject _ False (PSInteger n) = show n
formatObject _ False (PSReal r) = show r
formatObject _ False (PSBoolean b) = if b then "true" else "false"
formatObject _ False _ = "--nostringval--"
formatObject heap True obj = case obj of
  PSInteger n -> show n
  PSReal r -> show r
  PSBoolean b -> if b then "true" else "false"
  PSString s -> "(" ++ s ++ ")"
  PSName s -> "/" ++ s
  PSSymbol s -> s
  PSNull -> "null"
  PSMark -> "-mark-"
  PSDict _ -> "-dict-"
  PSBlock objs -> "{" ++ unwords (map (formatObject heap True) objs) ++ "}"
  PSArray aid -> "[" ++ unwords (map (formatObject heap True) (Map.findWithDefault [] aid heap)) ++ "]"
