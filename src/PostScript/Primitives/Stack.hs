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
    psStack,
    psPrintEq,
    psPrintDebug,
    psPrint,
    psQuit,
    psArray,
    psAload,
    psAstore,
    psCvx,
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
    psType,
    psStringAlloc,
    psCvlit,
    psCvn,
    psXcheck,
    psCvs,
    psCvrs,
    psAnchorsearch,
    psSearch,
    formatObject,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (gets, modify)
import Data.Char (intToDigit, toUpper)
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Numeric (showIntAtBase)
import PostScript.Environment (isBuiltin, loopWhile, newArray, newDict, peek, pop, popTwo, push, toCode)
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
        PSBlock xs -> do
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
    PSBlock objs -> push (PSInteger (length objs))
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
    (PSBlock xs, PSInteger i) ->
      if i >= 0 && i < length xs then push (xs !! i) else error "block index out of bounds"
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
    (PSString s, PSInteger i) -> do
      valByte <- case val of
        PSInteger n -> return (toEnum (fromIntegral n `mod` 256) :: Char)
        _ -> error "string put requires integer byte value"
      if i >= 0 && i < length s
        then mutateStringInState c (take i s ++ [valByte] ++ drop (i + 1) s)
        else error "string put index out of bounds"
    _ -> error "invalid arguments to put"

psForall :: ([Object] -> Interpreter ()) -> Interpreter ()
psForall eval = do
  code <- toCode <$> pop
  pop >>= \case
    PSArray aid -> do
      xs <- gets (Map.findWithDefault [] aid . arrayHeap)
      loopWhile [push x >> eval code | x <- xs]
    PSDict did -> do
      kvs <- gets (Map.findWithDefault Map.empty did . dictHeap)
      loopWhile [push (PSName k) >> push v >> eval code | (k, v) <- Map.toList kvs]
    _ -> error "forall requires array or dict"

-- 'aload': array aload -> a0 ... an-1 array (unpacks array elements and pushes array back)
psAload :: Interpreter ()
psAload = do
  arrObj <- pop
  case arrObj of
    PSArray aid -> do
      xs <- gets (Map.findWithDefault [] aid . arrayHeap)
      mapM_ push xs
      push arrObj
    _ -> error "typecheck: aload expected array"

-- 'astore': any_0 ... any_{n-1} array astore -> array
psAstore :: Interpreter ()
psAstore = do
  arrObj <- pop
  case arrObj of
    PSArray aid -> do
      xs <- gets (Map.findWithDefault [] aid . arrayHeap)
      let n = length xs
      st <- gets operandStack
      if length st < n
        then error "stackunderflow in astore"
        else do
          let (popped, remaining) = splitAt n st
              newContents = reverse popped
          modify (\s -> s {arrayHeap = Map.insert aid newContents (arrayHeap s), operandStack = arrObj : remaining})
    _ -> error "typecheck: astore expected array"

-- 'cvx': any cvx -> any (converts literal array/name to executable block/symbol)
psCvx :: Interpreter ()
psCvx =
  pop >>= \case
    PSArray aid -> do
      xs <- gets (Map.findWithDefault [] aid . arrayHeap)
      push (PSBlock xs)
    PSName s
      | isBuiltin s -> push (PSOperator s)
      | otherwise -> push (PSSymbol s)
    other -> push other

-- Delimiters and Marks: [, ], <<, >>, counttomark, cleartomark
psMark :: Interpreter ()
psMark = push PSMark

psCountToMark :: Interpreter ()
psCountToMark = do
  st <- gets operandStack
  case break (== PSMark) st of
    (items, PSMark : _) -> push (PSInteger (length items))
    _ -> error "unmatched mark on counttomark"

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

psStack :: Interpreter ()
psStack = do
  st <- gets operandStack
  h <- gets arrayHeap
  mapM_ (\o -> outputStr (formatObject h False o ++ "\n")) st

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
formatObject _ False (PSOperator s) = s
formatObject _ False (PSInteger n) = show n
formatObject _ False (PSReal r) = show r
formatObject _ False (PSBoolean b) = if b then "true" else "false"
formatObject _ False _ = "--nostringval--"
formatObject heap True obj = case obj of
  PSInteger n -> show n
  PSReal r -> show r
  PSBoolean b -> if b then "true" else "false"
  PSString s -> "(" ++ escapeString s ++ ")"
  PSName s -> "/" ++ s
  PSSymbol s -> s
  PSOperator op -> "--" ++ op ++ "--"
  PSNull -> "null"
  PSMark -> "-mark-"
  PSDict _ -> "-dict-"
  PSSave _ -> "-savelevel-"
  PSBlock objs -> "{" ++ unwords (map (formatObject heap True) objs) ++ "}"
  PSArray aid -> "[" ++ unwords (map (formatObject heap True) (Map.findWithDefault [] aid heap)) ++ "]"

escapeString :: String -> String
escapeString [] = []
escapeString ('\0' : cs) = "\\000" ++ escapeString cs
escapeString ('(' : cs) = "\\(" ++ escapeString cs
escapeString (')' : cs) = "\\)" ++ escapeString cs
escapeString ('\\' : cs) = "\\\\" ++ escapeString cs
escapeString (c : cs) = c : escapeString cs

-- 'type': any type -> name
psType :: Interpreter ()
psType =
  pop >>= \obj -> push (PSSymbol (typeName obj))
  where
    typeName (PSInteger _) = "integertype"
    typeName (PSReal _) = "realtype"
    typeName (PSBoolean _) = "booleantype"
    typeName (PSString _) = "stringtype"
    typeName (PSArray _) = "arraytype"
    typeName (PSDict _) = "dicttype"
    typeName (PSSave _) = "savetype"
    typeName (PSName _) = "nametype"
    typeName (PSSymbol _) = "nametype"
    typeName (PSOperator _) = "operatortype"
    typeName (PSBlock _) = "arraytype"
    typeName PSNull = "nulltype"
    typeName PSMark = "marktype"

-- 'string': int string -> string (allocate empty string of given length)
psStringAlloc :: Interpreter ()
psStringAlloc =
  pop >>= \case
    PSInteger n | n >= 0 -> push (PSString (replicate n '\0'))
    _ -> error "string length must be non-negative integer"

-- 'cvlit': any cvlit -> any (make object literal)
psCvlit :: Interpreter ()
psCvlit =
  pop >>= \case
    PSSymbol s -> push (PSName s)
    PSBlock objs -> newArray objs >>= push
    other -> push other

-- 'cvn': string|name cvn -> name
psCvn :: Interpreter ()
psCvn =
  pop >>= \case
    PSString s -> push (PSName s)
    PSName s -> push (PSName s)
    PSSymbol s -> push (PSName s)
    _ -> error "cvn requires string or name"

-- 'xcheck': any xcheck -> bool (tests if object is executable)
psXcheck :: Interpreter ()
psXcheck =
  pop >>= \case
    PSBlock _ -> push (PSBoolean True)
    PSSymbol _ -> push (PSBoolean True)
    PSOperator _ -> push (PSBoolean True)
    _ -> push (PSBoolean False)

-- 'cvrs': num radix string cvrs -> substring
psCvrs :: Interpreter ()
psCvrs = do
  strObj <- pop
  radixObj <- pop
  numObj <- pop
  case (strObj, radixObj, numObj) of
    (PSString s, PSInteger radix, num) | radix >= 2 && radix <= 36 -> do
      let formatted = case num of
            PSInteger n
              | n < 0 -> '-' : map toUpper (showIntAtBase radix intToDigit (negate n) "")
              | otherwise -> map toUpper (showIntAtBase radix intToDigit n "")
            PSReal r
              | radix == 10 -> show r
              | fromIntegral (round r :: Int) == r ->
                  let n = round r :: Int
                   in if n < 0
                        then '-' : map toUpper (showIntAtBase radix intToDigit (negate n) "")
                        else map toUpper (showIntAtBase radix intToDigit n "")
              | otherwise -> show r
            _ -> error "cvrs expected number"
      let newS = formatted ++ drop (length formatted) s
      mutateStringInState strObj newS
      push (PSString formatted)
    _ -> error "cvrs arguments mismatch"

-- 'anchorsearch': string seek anchorsearch -> post match true OR string false
psAnchorsearch :: Interpreter ()
psAnchorsearch = do
  seekObj <- pop
  strObj <- pop
  case (strObj, seekObj) of
    (PSString s, PSString seek) ->
      if seek `isPrefixOf` s
        then do
          push (PSString (drop (length seek) s))
          push (PSString seek)
          push (PSBoolean True)
        else do
          push (PSString s)
          push (PSBoolean False)
    _ -> error "anchorsearch requires strings"

-- 'search': string seek search -> post match pre true OR string false
psSearch :: Interpreter ()
psSearch = do
  seekObj <- pop
  strObj <- pop
  case (strObj, seekObj) of
    (PSString s, PSString seek) ->
      case findSub seek s of
        Just (pre, match, post) -> do
          push (PSString post)
          push (PSString match)
          push (PSString pre)
          push (PSBoolean True)
        Nothing -> do
          push (PSString s)
          push (PSBoolean False)
    _ -> error "search requires strings"
  where
    findSub needle haystack = go "" haystack
      where
        go _ [] = if null needle then Just ("", "", "") else Nothing
        go acc str@(c : cs)
          | needle `isPrefixOf` str = Just (reverse acc, needle, drop (length needle) str)
          | otherwise = go (c : acc) cs

-- 'cvs': any string cvs -> substring
psCvs :: Interpreter ()
psCvs = do
  strObj <- pop
  valObj <- pop
  case strObj of
    PSString s -> do
      h <- gets arrayHeap
      let formatted = formatObject h False valObj
          newS = formatted ++ drop (length formatted) s
      mutateStringInState strObj newS
      push (PSString formatted)
    _ -> error "cvs requires string"

-- Mutates string in-place across stack and dictionaries
mutateStringInState :: Object -> String -> Interpreter ()
mutateStringInState oldObj newContent =
  modify
    ( \st ->
        st
          { operandStack = map (\o -> if o == oldObj then PSString newContent else o) (operandStack st),
            dictHeap = fmap (fmap (\o -> if o == oldObj then PSString newContent else o)) (dictHeap st)
          }
    )
