module Main (main) where

import Data.Bits qualified as Bits
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf, isSuffixOf, sort)
import Data.Map.Strict qualified as Map
import PostScript (executeScript)
import PostScript.Evaluator (runInterpreter)
import PostScript.Lexer (parsePostScript)
import PostScript.Primitives.Stack (formatObject)
import PostScript.Types (Object (..), PSState (..), initPSState)
import System.Directory (listDirectory)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.FilePath (takeFileName, (</>))
import System.Process (readProcessWithExitCode)
import Test.QuickCheck
import Text.Read (readMaybe)

-- | Parse and evaluate PostScript code, capturing stdout and returning (output, finalStack).
evalScript :: String -> IO (Either String (String, [Object]))
evalScript code = case parsePostScript code of
  Left err -> pure (Left ("Parse error: " ++ err))
  Right objs -> do
    outRef <- newIORef []
    let s0 =
          initPSState
            { printAction = \s -> modifyIORef' outRef (s :)
            }
    res <- runInterpreter objs s0
    case res of
      Left err -> pure (Left err)
      Right st -> do
        out <- concat . reverse <$> readIORef outRef
        pure (Right (out, operandStack st))

-- | Read and execute a .ps file from disk.
evalFile :: FilePath -> IO (Either String (String, [Object]))
evalFile path = readFile path >>= evalScript

-- | Helper for asserting that running a script leaves expected objects on the stack.
runsTo :: String -> [Object] -> Property
runsTo code expected = ioProperty $ do
  res <- evalScript code
  pure (fmap snd res == Right expected)

-- QuickCheck Properties - Stack, Math, Control, Dict Basics
prop_dupPop :: Int -> Property
prop_dupPop x = (show x ++ " dup pop") `runsTo` [PSInteger x]

prop_exchTwice :: Int -> Int -> Property
prop_exchTwice x y = (show x ++ " " ++ show y ++ " exch exch") `runsTo` [PSInteger y, PSInteger x]

prop_dupAddDoubles :: Int -> Property
prop_dupAddDoubles x = (show x ++ " dup add") `runsTo` [PSInteger (x * 2)]

prop_countMatches :: [Int] -> Property
prop_countMatches xs =
  let script = unwords (map show xs) ++ " count"
   in ioProperty $ do
        res <- evalScript script
        pure
          ( case fmap snd res of
              Right (PSInteger c : _) -> c == length xs
              _ -> False
          )

prop_clearEmpties :: [Int] -> Property
prop_clearEmpties xs = (unwords (map show xs) ++ " clear") `runsTo` []

prop_addCommutative :: Int -> Int -> Property
prop_addCommutative x y = ioProperty $ do
  r1 <- evalScript (show x ++ " " ++ show y ++ " add")
  r2 <- evalScript (show y ++ " " ++ show x ++ " add")
  pure (fmap snd r1 == Right [PSInteger (x + y)] && r1 == r2)

prop_mulCommutative :: Int -> Int -> Property
prop_mulCommutative x y = ioProperty $ do
  r1 <- evalScript (show x ++ " " ++ show y ++ " mul")
  r2 <- evalScript (show y ++ " " ++ show x ++ " mul")
  pure (fmap snd r1 == Right [PSInteger (x * y)] && r1 == r2)

prop_subInverse :: Int -> Int -> Property
prop_subInverse x y = (show x ++ " " ++ show y ++ " sub " ++ show y ++ " add") `runsTo` [PSInteger x]

prop_negInvolution :: Int -> Property
prop_negInvolution x = (show x ++ " neg neg") `runsTo` [PSInteger x]

prop_absNonNegative :: Int -> Property
prop_absNonNegative x = (show x ++ " abs") `runsTo` [PSInteger (abs x)]

prop_idivModIdentity :: Int -> NonZero Int -> Property
prop_idivModIdentity x (NonZero y) =
  (show x ++ " " ++ show y ++ " idiv " ++ show y ++ " mul " ++ show x ++ " " ++ show y ++ " mod add")
    `runsTo` [PSInteger x]

prop_ifelseTrue :: Int -> Int -> Property
prop_ifelseTrue x y = ("true { " ++ show x ++ " } { " ++ show y ++ " } ifelse") `runsTo` [PSInteger x]

prop_ifelseFalse :: Int -> Int -> Property
prop_ifelseFalse x y = ("false { " ++ show x ++ " } { " ++ show y ++ " } ifelse") `runsTo` [PSInteger y]

prop_repeatCount :: NonNegative (Small Int) -> Property
prop_repeatCount (NonNegative (Small n)) = ("0 " ++ show n ++ " { 1 add } repeat") `runsTo` [PSInteger n]

prop_defLookup :: Int -> Property
prop_defLookup x = ("/x " ++ show x ++ " def x") `runsTo` [PSInteger x]

prop_eqReflexive :: Int -> Property
prop_eqReflexive x = (show x ++ " " ++ show x ++ " eq") `runsTo` [PSBoolean True]

prop_quitTerminates :: Int -> Int -> Property
prop_quitTerminates x y = (show x ++ " quit " ++ show y) `runsTo` [PSInteger x]

prop_formatEq :: Int -> String -> Bool
prop_formatEq x s =
  formatObject Map.empty False (PSInteger x) == show x
    && formatObject Map.empty False (PSString s) == s

prop_formatDebug :: String -> Bool
prop_formatDebug s =
  let safeStr = filter (`notElem` "()\\\0") s
   in formatObject Map.empty True (PSString safeStr) == "(" ++ safeStr ++ ")"

prop_arrayAllocLength :: NonNegative (Small Int) -> Property
prop_arrayAllocLength (NonNegative (Small n)) =
  (show n ++ " array length") `runsTo` [PSInteger n]

prop_arrayPutGet :: NonNegative (Small Int) -> Int -> Property
prop_arrayPutGet (NonNegative (Small n)) val =
  let size = max 1 (n + 1)
      idx = n `mod` size
      script = show size ++ " array dup " ++ show idx ++ " " ++ show val ++ " put " ++ show idx ++ " get"
   in script `runsTo` [PSInteger val]

prop_bracketArray :: Int -> Int -> Int -> Property
prop_bracketArray a b c =
  ("[" ++ show a ++ " " ++ show b ++ " " ++ show c ++ "] length") `runsTo` [PSInteger 3]

prop_bracketArrayGet :: Int -> Int -> Int -> Property
prop_bracketArrayGet a b c =
  ("[" ++ show a ++ " " ++ show b ++ " " ++ show c ++ "] 1 get") `runsTo` [PSInteger b]

prop_dictPutGet :: Int -> Property
prop_dictPutGet val =
  ("1 dict dup /key " ++ show val ++ " put /key get") `runsTo` [PSInteger val]

prop_dictLiteral :: Int -> Int -> Property
prop_dictLiteral a b =
  ("<</x " ++ show a ++ " /y " ++ show b ++ ">> dup /x get exch /y get") `runsTo` [PSInteger b, PSInteger a]

prop_dictLength :: Int -> Int -> Property
prop_dictLength a b =
  ("<</x " ++ show a ++ " /y " ++ show b ++ ">> length") `runsTo` [PSInteger 2]

prop_arrayCopy :: Int -> Int -> Property
prop_arrayCopy a b =
  ("[" ++ show a ++ "] [" ++ show b ++ "] copy 0 get") `runsTo` [PSInteger a]

prop_arrayForallSum :: NonNegative (Small Int) -> NonNegative (Small Int) -> Property
prop_arrayForallSum (NonNegative (Small a)) (NonNegative (Small b)) =
  ("0 [" ++ show a ++ " " ++ show b ++ "] {add} forall") `runsTo` [PSInteger (a + b)]

prop_nullEq :: Property
prop_nullEq = "null null eq" `runsTo` [PSBoolean True]

prop_arrayRefSemantics :: Int -> Property
prop_arrayRefSemantics val =
  ("1 array dup 0 " ++ show val ++ " put 0 get") `runsTo` [PSInteger val]

-- Bitwise and Mark Stack Properties
prop_bitwiseAnd :: Int -> Int -> Property
prop_bitwiseAnd x y = (show x ++ " " ++ show y ++ " and") `runsTo` [PSInteger (x Bits..&. y)]

prop_bitwiseOr :: Int -> Int -> Property
prop_bitwiseOr x y = (show x ++ " " ++ show y ++ " or") `runsTo` [PSInteger (x Bits..|. y)]

prop_bitwiseXor :: Int -> Int -> Property
prop_bitwiseXor x y = (show x ++ " " ++ show y ++ " xor") `runsTo` [PSInteger (x `Bits.xor` y)]

prop_countToMark :: Int -> Int -> Property
prop_countToMark a b = ("mark " ++ show a ++ " " ++ show b ++ " counttomark") `runsTo` [PSInteger 2, PSInteger b, PSInteger a, PSMark]

prop_clearToMark :: Int -> Int -> Property
prop_clearToMark a b = ("mark " ++ show a ++ " " ++ show b ++ " cleartomark") `runsTo` []

-- Control Flow & Procedures (exec, loop, exit, bind)
prop_execBlock :: Int -> Int -> Property
prop_execBlock a b = ("{" ++ show a ++ " " ++ show b ++ " add} exec") `runsTo` [PSInteger (a + b)]

prop_execLiteral :: Int -> Property
prop_execLiteral a = (show a ++ " exec") `runsTo` [PSInteger a]

prop_loopExit :: Positive (Small Int) -> Property
prop_loopExit (Positive (Small n)) =
  let target = (n `mod` 30) + 1
   in ("0 { 1 add dup " ++ show target ++ " eq { exit } if } loop") `runsTo` [PSInteger target]

prop_repeatExit :: Int -> Property
prop_repeatExit val = ("1000 {" ++ show val ++ " exit } repeat") `runsTo` [PSInteger val]

prop_forExit :: Positive (Small Int) -> Property
prop_forExit (Positive (Small n)) =
  let limit = (n `mod` 15) + 2
   in ("1 1 100 { dup " ++ show limit ++ " eq { exit } if } for") `runsTo` map PSInteger [limit, limit - 1 .. 1]

prop_bindOperator :: Property
prop_bindOperator = "{ add } bind" `runsTo` [PSBlock [PSOperator "add"]]

prop_bindPreservesLiterals :: Int -> Property
prop_bindPreservesLiterals a = ("{ " ++ show a ++ " add } bind") `runsTo` [PSBlock [PSInteger a, PSOperator "add"]]

prop_bindExecutes :: Int -> Int -> Property
prop_bindExecutes a b = ("{ " ++ show a ++ " " ++ show b ++ " add } bind exec") `runsTo` [PSInteger (a + b)]

-- 2D Coordinate System & Matrix Transformations
prop_matrixAllocLength :: Property
prop_matrixAllocLength = "matrix length" `runsTo` [PSInteger 6]

prop_identmatrixIdentity :: Property
prop_identmatrixIdentity = "matrix identmatrix dup 0 get exch 3 get" `runsTo` [PSReal 1.0, PSReal 1.0]

prop_defaultmatrixIdentity :: Property
prop_defaultmatrixIdentity = "matrix defaultmatrix dup 0 get exch 3 get" `runsTo` [PSReal 1.0, PSReal 1.0]

prop_translateMatrix :: Small Int -> Small Int -> Property
prop_translateMatrix (Small tx) (Small ty) =
  (show tx ++ " " ++ show ty ++ " matrix translate dup 4 get exch 5 get")
    `runsTo` [PSReal (fromIntegral ty), PSReal (fromIntegral tx)]

prop_scaleMatrix :: Small Int -> Small Int -> Property
prop_scaleMatrix (Small sx) (Small sy) =
  (show sx ++ " " ++ show sy ++ " matrix scale dup 0 get exch 3 get")
    `runsTo` [PSReal (fromIntegral sy), PSReal (fromIntegral sx)]

prop_rotateMatrix :: Property
prop_rotateMatrix = "0 matrix rotate dup 0 get exch 3 get" `runsTo` [PSReal 1.0, PSReal 1.0]

prop_rotateCTM :: Property
prop_rotateCTM = ioProperty $ do
  res <- evalScript "90 rotate 10 0 transform"
  pure $ case res of
    Right (_, [PSReal y, PSReal x]) -> abs x < 1e-4 && abs (y - 10.0) < 1e-4
    _ -> False

prop_concatCTM :: Small Int -> Small Int -> Property
prop_concatCTM (Small tx) (Small ty) =
  (show tx ++ " " ++ show ty ++ " matrix translate concat 0 0 transform")
    `runsTo` [PSReal (fromIntegral ty), PSReal (fromIntegral tx)]

prop_concatmatrixIdentity :: Property
prop_concatmatrixIdentity = "matrix matrix matrix concatmatrix 0 get" `runsTo` [PSReal 1.0]

prop_invertMatrixIdentity :: Property
prop_invertMatrixIdentity = "matrix matrix invertmatrix 0 get" `runsTo` [PSReal 1.0]

prop_dtransformIgnoresTranslation :: Small Int -> Small Int -> Property
prop_dtransformIgnoresTranslation (Small dx) (Small dy) =
  (show dx ++ " " ++ show dy ++ " 100 200 translate dtransform")
    `runsTo` [PSReal (fromIntegral dy), PSReal (fromIntegral dx)]

prop_dtransformIdtransformRoundTrip :: Small Int -> Small Int -> Property
prop_dtransformIdtransformRoundTrip (Small dx) (Small dy) = ioProperty $ do
  let script = show dx ++ " " ++ show dy ++ " 2 3 scale dtransform idtransform"
  res <- evalScript script
  pure $ case res of
    Right (_, [PSReal dy', PSReal dx']) ->
      abs (fromIntegral dy - dy') < 1e-6 && abs (fromIntegral dx - dx') < 1e-6
    _ -> False

prop_currentmatrixSetmatrix :: Property
prop_currentmatrixSetmatrix =
  "10 20 translate matrix currentmatrix initmatrix setmatrix 0 0 transform"
    `runsTo` [PSReal 20.0, PSReal 10.0]

prop_transformItransformRoundTrip :: Small Int -> Small Int -> Property
prop_transformItransformRoundTrip (Small x) (Small y) = ioProperty $ do
  let script = show x ++ " " ++ show y ++ " 50 100 translate transform itransform"
  res <- evalScript script
  pure $ case res of
    Right (_, [PSReal y', PSReal x']) ->
      abs (fromIntegral y - y') < 1e-6 && abs (fromIntegral x - x') < 1e-6
    _ -> False

-- Path Construction & Geometry
prop_movetoCurrentpoint :: Small Int -> Small Int -> Property
prop_movetoCurrentpoint (Small x) (Small y) =
  (show x ++ " " ++ show y ++ " moveto currentpoint")
    `runsTo` [PSReal (fromIntegral y), PSReal (fromIntegral x)]

prop_rmovetoRel :: Small Int -> Small Int -> Small Int -> Small Int -> Property
prop_rmovetoRel (Small x) (Small y) (Small dx) (Small dy) =
  (show x ++ " " ++ show y ++ " moveto " ++ show dx ++ " " ++ show dy ++ " rmoveto currentpoint")
    `runsTo` [PSReal (fromIntegral (y + dy)), PSReal (fromIntegral (x + dx))]

prop_linetoCurrentpoint :: Small Int -> Small Int -> Property
prop_linetoCurrentpoint (Small x) (Small y) =
  ("0 0 moveto " ++ show x ++ " " ++ show y ++ " lineto currentpoint")
    `runsTo` [PSReal (fromIntegral y), PSReal (fromIntegral x)]

prop_rlinetoRel :: Small Int -> Small Int -> Small Int -> Small Int -> Property
prop_rlinetoRel (Small x) (Small y) (Small dx) (Small dy) =
  (show x ++ " " ++ show y ++ " moveto " ++ show dx ++ " " ++ show dy ++ " rlineto currentpoint")
    `runsTo` [PSReal (fromIntegral (y + dy)), PSReal (fromIntegral (x + dx))]

prop_closepathReturnsToStart :: Small Int -> Small Int -> Property
prop_closepathReturnsToStart (Small x) (Small y) =
  (show x ++ " " ++ show y ++ " moveto 10 20 rlineto 30 40 rlineto closepath currentpoint")
    `runsTo` [PSReal (fromIntegral y), PSReal (fromIntegral x)]

prop_newpathClearsCurrentpoint :: Property
prop_newpathClearsCurrentpoint = ioProperty $ do
  res <- evalScript "10 20 moveto newpath currentpoint"
  pure $ case res of
    Left err -> "nocurrentpoint" `isInfixOf` err
    Right _ -> False

prop_curvetoEndpoint :: Small Int -> Small Int -> Property
prop_curvetoEndpoint (Small x3) (Small y3) =
  ("0 0 moveto 10 20 30 40 " ++ show x3 ++ " " ++ show y3 ++ " curveto currentpoint")
    `runsTo` [PSReal (fromIntegral y3), PSReal (fromIntegral x3)]

prop_rcurvetoEndpoint :: Small Int -> Small Int -> Property
prop_rcurvetoEndpoint (Small dx3) (Small dy3) =
  ("10 10 moveto 5 5 10 10 " ++ show dx3 ++ " " ++ show dy3 ++ " rcurveto currentpoint")
    `runsTo` [PSReal (fromIntegral (10 + dy3)), PSReal (fromIntegral (10 + dx3))]

prop_arcEndpoint :: Property
prop_arcEndpoint = ioProperty $ do
  res <- evalScript "100 100 50 0 90 arc currentpoint"
  pure $ case res of
    Right (_, [PSReal y, PSReal x]) -> abs (x - 100.0) < 1e-4 && abs (y - 150.0) < 1e-4
    _ -> False

prop_arcnEndpoint :: Property
prop_arcnEndpoint = ioProperty $ do
  res <- evalScript "100 100 50 90 0 arcn currentpoint"
  pure $ case res of
    Right (_, [PSReal y, PSReal x]) -> abs (x - 150.0) < 1e-4 && abs (y - 100.0) < 1e-4
    _ -> False

prop_arctoTangents :: Property
prop_arctoTangents = ioProperty $ do
  res <- evalScript "0 0 moveto 0 100 100 100 50 arcto"
  pure $ case res of
    Right (_, [PSReal yt2, PSReal xt2, PSReal yt1, PSReal xt1]) ->
      abs xt1 < 1e-4
        && abs (yt1 - 50.0) < 1e-4
        && abs (xt2 - 50.0) < 1e-4
        && abs (yt2 - 100.0) < 1e-4
    _ -> False

prop_pathbboxRect :: Property
prop_pathbboxRect =
  "newpath 10 20 moveto 50 20 lineto 50 80 lineto 10 80 lineto closepath pathbbox"
    `runsTo` [PSReal 80.0, PSReal 50.0, PSReal 20.0, PSReal 10.0]

prop_flattenpathPreservesBBox :: Property
prop_flattenpathPreservesBBox =
  "newpath 0 0 moveto 100 100 50 0 90 arc flattenpath pathbbox"
    `runsTo` [PSReal 150.0, PSReal 150.0, PSReal 0.0, PSReal 0.0]

prop_pathforallAccumulates :: Property
prop_pathforallAccumulates =
  "0 newpath 10 20 moveto 30 40 lineto closepath { add add } { add add } {} { 1 add } pathforall"
    `runsTo` [PSReal 101.0]

prop_currentpointWithCTM :: Small Int -> Small Int -> Property
prop_currentpointWithCTM (Small tx) (Small ty) =
  ("100 200 moveto " ++ show tx ++ " " ++ show ty ++ " translate currentpoint")
    `runsTo` [PSReal (200.0 - fromIntegral ty), PSReal (100.0 - fromIntegral tx)]

-- Graphics State, Color, Painting & SVG Rendering
prop_gsaveGrestoreRGB :: Property
prop_gsaveGrestoreRGB =
  "1 0 0 setrgbcolor gsave 0 1 0 setrgbcolor grestore currentrgbcolor"
    `runsTo` [PSReal 0.0, PSReal 0.0, PSReal 1.0]

prop_grestoreall :: Property
prop_grestoreall =
  "0.2 0.3 0.4 setrgbcolor gsave 0.5 setgray gsave 1 0 0 setrgbcolor grestoreall currentrgbcolor"
    `runsTo` [PSReal 0.4, PSReal 0.3, PSReal 0.2]

prop_setrgbcolor :: Property
prop_setrgbcolor =
  "0.1 0.5 0.9 setrgbcolor currentrgbcolor"
    `runsTo` [PSReal 0.9, PSReal 0.5, PSReal 0.1]

prop_setgrayCurrentgray :: Property
prop_setgrayCurrentgray =
  "0.65 setgray currentgray"
    `runsTo` [PSReal 0.65]

prop_setcmykcolor :: Property
prop_setcmykcolor =
  "0 0 0 1 setcmykcolor currentrgbcolor"
    `runsTo` [PSReal 0.0, PSReal 0.0, PSReal 0.0]

prop_sethsbcolor :: Property
prop_sethsbcolor =
  "0 0 1 sethsbcolor currentrgbcolor"
    `runsTo` [PSReal 1.0, PSReal 1.0, PSReal 1.0]

prop_setlinewidth :: Small Int -> Property
prop_setlinewidth (Small w) =
  let w' = abs w + 1
   in (show w' ++ " setlinewidth currentlinewidth") `runsTo` [PSReal (fromIntegral w')]

prop_strokeClearsPath :: Property
prop_strokeClearsPath = ioProperty $ do
  res <- evalScript "newpath 0 0 moveto 10 10 lineto stroke currentpoint"
  pure $ case res of
    Left err -> "nocurrentpoint" `isInfixOf` err
    Right _ -> False

prop_fillClearsPath :: Property
prop_fillClearsPath = ioProperty $ do
  res <- evalScript "newpath 0 0 moveto 10 10 lineto 0 10 lineto closepath fill currentpoint"
  pure $ case res of
    Left err -> "nocurrentpoint" `isInfixOf` err
    Right _ -> False

prop_eofillClearsPath :: Property
prop_eofillClearsPath = ioProperty $ do
  res <- evalScript "newpath 0 0 moveto 10 10 lineto 0 10 lineto closepath eofill currentpoint"
  pure $ case res of
    Left err -> "nocurrentpoint" `isInfixOf` err
    Right _ -> False

prop_clipPreservesCurrentpoint :: Property
prop_clipPreservesCurrentpoint =
  "newpath 0 0 moveto 10 20 lineto clip currentpoint"
    `runsTo` [PSReal 20.0, PSReal 10.0]

prop_showpageClearsPath :: Property
prop_showpageClearsPath = ioProperty $ do
  res <- evalScript "newpath 0 0 moveto 10 10 lineto showpage currentpoint"
  pure $ case res of
    Left err -> "nocurrentpoint" `isInfixOf` err
    Right _ -> False

prop_strokeEmitsSVG :: Property
prop_strokeEmitsSVG = ioProperty $ do
  res <- executeScript "1 0 0 setrgbcolor 2 setlinewidth newpath 0 0 moveto 100 200 lineto stroke"
  pure $ case res of
    Right st ->
      case canvasElements st of
        [elemStr] -> "stroke=\"rgb(255,0,0)\"" `isInfixOf` elemStr && "stroke-width=\"2\"" `isInfixOf` elemStr
        _ -> False
    Left _ -> False

prop_fillEmitsSVG :: Property
prop_fillEmitsSVG = ioProperty $ do
  res <- executeScript "0 1 0 setrgbcolor newpath 0 0 moveto 100 0 lineto 100 100 lineto closepath fill"
  pure $ case res of
    Right st ->
      case canvasElements st of
        [elemStr] -> "fill=\"rgb(0,255,0)\"" `isInfixOf` elemStr && "fill-rule" `notElem` words elemStr
        _ -> False
    Left _ -> False

prop_eofillEmitsSVG :: Property
prop_eofillEmitsSVG = ioProperty $ do
  res <- executeScript "newpath 0 0 moveto 100 0 lineto 100 100 lineto closepath eofill"
  pure $ case res of
    Right st ->
      case canvasElements st of
        [elemStr] -> "fill-rule=\"evenodd\"" `isInfixOf` elemStr
        _ -> False
    Left _ -> False

-- New Utility Operator Properties
prop_cviTruncatesReal :: Property
prop_cviTruncatesReal =
  "3.7 cvi -2.9 cvi (42) cvi"
    `runsTo` [PSInteger 42, PSInteger (-2), PSInteger 3]

prop_randGeneratesPositiveInt :: Property
prop_randGeneratesPositiveInt = ioProperty $ do
  res <- evalScript "rand rand"
  pure $ case res of
    Right (_, [PSInteger r2, PSInteger r1]) -> r1 >= 0 && r2 >= 0
    _ -> False

prop_aloadUnpacksArray :: Property
prop_aloadUnpacksArray = ioProperty $ do
  res <- evalScript "[10 20 30] aload"
  pure $ case res of
    Right (_, [PSArray _, PSInteger 30, PSInteger 20, PSInteger 10]) -> True
    _ -> False

prop_cvxExecutesBlock :: Property
prop_cvxExecutesBlock =
  "[10 20 add] cvx exec" `runsTo` [PSInteger 30]

prop_loadAliasesBuiltin :: Property
prop_loadAliasesBuiltin =
  "/m /moveto load def newpath 15 25 m currentpoint"
    `runsTo` [PSReal 25.0, PSReal 15.0]

prop_whereFindsDict :: Property
prop_whereFindsDict = ioProperty $ do
  res <- evalScript "/myVar 99 def /myVar where /nonexistent where"
  pure $ case res of
    Right (_, [PSBoolean False, PSBoolean True, PSDict _]) -> True
    _ -> False

prop_saveRestorePreservesState :: Property
prop_saveRestorePreservesState =
  "10 setlinewidth save 50 setlinewidth restore currentlinewidth"
    `runsTo` [PSReal 10.0]

prop_clippathEmitsBoundary :: Property
prop_clippathEmitsBoundary =
  "clippath pathbbox"
    `runsTo` [PSReal 792.0, PSReal 612.0, PSReal 0.0, PSReal 0.0]

prop_bitshiftShiftsBits :: Property
prop_bitshiftShiftsBits =
  "8 2 bitshift 32 -2 bitshift"
    `runsTo` [PSInteger 8, PSInteger 32]

prop_astorePopsIntoArray :: Property
prop_astorePopsIntoArray = ioProperty $ do
  res <- evalScript "10 20 30 3 array astore aload"
  pure $ case res of
    Right (_, [PSArray _, PSInteger 30, PSInteger 20, PSInteger 10]) -> True
    _ -> False

prop_cleardictstackResetsStack :: Property
prop_cleardictstackResetsStack = ioProperty $ do
  res <- evalScript "5 dict begin 5 dict begin cleardictstack userdict"
  pure $ case res of
    Right (_, [PSDict 0]) -> True
    _ -> False

prop_usertimeReturnsInt :: Property
prop_usertimeReturnsInt = ioProperty $ do
  res <- evalScript "usertime"
  pure $ case res of
    Right (_, [PSInteger t]) -> t >= 0
    _ -> False

prop_userdictPushesRootDict :: Property
prop_userdictPushesRootDict = ioProperty $ do
  res <- evalScript "userdict"
  pure $ case res of
    Right (_, [PSDict 0]) -> True
    _ -> False

prop_trailingDotParsesReal :: Property
prop_trailingDotParsesReal =
  "72. 300. div"
    `runsTo` [PSReal 0.24]

-- Section 2: Integration tests verifying final operand stack
testPrograms :: [(FilePath, [Object])]
testPrograms =
  [ ("fibonacci.ps", [PSBoolean True, PSInteger 33, PSInteger 55]),
    ("primes.ps", [PSBoolean False, PSBoolean True, PSInteger 129, PSInteger 10]),
    ("collatz_poly.ps", [PSInteger 22, PSInteger 111, PSInteger 8]),
    ("dict_scope.ps", [PSBoolean False, PSInteger 1, PSInteger 2, PSInteger 100, PSInteger 300, PSInteger 999, PSInteger 42, PSInteger 1199]),
    ("stack_ops.ps", [PSInteger 5, PSInteger 340, PSInteger 40, PSInteger 30, PSInteger 10, PSInteger 20]),
    ("math_advanced.ps", [PSInteger (-15), PSInteger 42, PSReal 3.4, PSInteger 2, PSInteger 3, PSReal (-3.0), PSReal 3.0, PSReal 3.0, PSReal 4.0, PSReal (-4.0), PSReal 3.0, PSReal (-3.0), PSReal 4.0, PSReal 0.0, PSReal 3.0, PSReal 256.0, PSReal 90.0, PSReal 45.0, PSBoolean True, PSBoolean True, PSReal 13.0, PSReal 5.0]),
    ("euclid_gcd.ps", [PSInteger 243, PSInteger 210, PSInteger 35, PSInteger 15, PSInteger 5040, PSInteger 720, PSInteger 60, PSInteger 36, PSInteger 12, PSInteger 35, PSInteger 6]),
    ("control_flow.ps", [PSInteger 31, PSInteger 30, PSInteger 60, PSBoolean True, PSBoolean True, PSBoolean True, PSBoolean True, PSBoolean True, PSInteger 1, PSInteger 0, PSInteger (-1)])
  ]

runProgramTest :: (FilePath, [Object]) -> IO Bool
runProgramTest (name, expected) = do
  putStr ("  * " ++ name ++ " ... ")
  res <- evalFile ("test/programs" </> name)
  case res of
    Right (_, stack)
      | stack == expected -> putStrLn "PASS" >> pure True
      | otherwise -> putStrLn "FAIL" >> pure False
    Left err -> putStrLn ("ERROR: " ++ err) >> pure False

-- Section 3: Differential testing against Ghostscript
runGhostscript :: FilePath -> IO (Either String String)
runGhostscript path = do
  (code, out, err) <- readProcessWithExitCode "gs" ["-q", "-dNODISPLAY", path] ""
  case code of
    ExitSuccess -> pure (Right out)
    ExitFailure _ -> pure (Left ("Ghostscript execution failed: " ++ err))

compareOutputs :: String -> String -> Bool
compareOutputs actual expected =
  let w1 = words actual
      w2 = words expected
   in length w1 == length w2
        && (and (zipWith matchToken w1 w2) || and (zipWith matchToken (sort w1) (sort w2)))
  where
    matchToken t1 t2 =
      t1 == t2 || case (readMaybe t1 :: Maybe Double, readMaybe t2 :: Maybe Double) of
        (Just d1, Just d2) -> abs (d1 - d2) < 0.005 || abs (d1 - d2) / max 1.0 (abs d2) < 0.005
        _ -> False

runDiffTest :: FilePath -> IO Bool
runDiffTest path = do
  let fileName = takeFileName path
  putStr ("  * " ++ fileName ++ " (vs Ghostscript) ... ")
  mMy <- fmap (fmap fst) (evalFile path)
  mGs <- runGhostscript path
  case (mMy, mGs) of
    (Right my, Right gs)
      | compareOutputs my gs -> putStrLn "PASS" >> pure True
      | otherwise -> putStrLn "FAIL" >> pure False
    (Left err, _) -> putStrLn ("ERROR: " ++ err) >> pure False
    (_, Left err) -> putStrLn ("GS ERROR: " ++ err) >> pure False

runQC :: (Testable prop) => String -> prop -> IO Bool
runQC lbl prop = do
  putStrLn ("  [QC] " ++ lbl)
  res <- quickCheckResult (withNumTests 100 prop)
  pure (isSuccess res)

main :: IO ()
main = do
  putStrLn "\n--- Section 1: Property-Based Testing (QuickCheck) ---"
  qcResults <-
    sequence
      [ runQC "Stack: dup pop preserves top value" prop_dupPop,
        runQC "Stack: exch twice preserves order" prop_exchTwice,
        runQC "Stack: dup add doubles value" prop_dupAddDoubles,
        runQC "Stack: count matches pushed items" prop_countMatches,
        runQC "Stack: clear empties operand stack" prop_clearEmpties,
        runQC "Math: addition is commutative" prop_addCommutative,
        runQC "Math: multiplication is commutative" prop_mulCommutative,
        runQC "Math: subtraction and addition are inverses" prop_subInverse,
        runQC "Math: double negation is identity" prop_negInvolution,
        runQC "Math: absolute value is non-negative" prop_absNonNegative,
        runQC "Math: integer division and modulo identity" prop_idivModIdentity,
        runQC "Control: ifelse evaluates true branch" prop_ifelseTrue,
        runQC "Control: ifelse evaluates false branch" prop_ifelseFalse,
        runQC "Control: repeat executes n times" prop_repeatCount,
        runQC "Scoping: variable definition and lookup" prop_defLookup,
        runQC "Relational: equality is reflexive" prop_eqReflexive,
        runQC "Session: quit stops execution immediately" prop_quitTerminates,
        runQC "Output: = formatting matches integer show and bare strings" prop_formatEq,
        runQC "Output: == formatting wraps strings in parentheses" prop_formatDebug,
        runQC "Array: allocation and length" prop_arrayAllocLength,
        runQC "Array: put and get by index" prop_arrayPutGet,
        runQC "Array: [ ... ] syntax and length" prop_bracketArray,
        runQC "Array: [ ... ] indexing with get" prop_bracketArrayGet,
        runQC "Dict: put and get by key" prop_dictPutGet,
        runQC "Dict: << ... >> literal and key retrieval" prop_dictLiteral,
        runQC "Dict: length matches key count" prop_dictLength,
        runQC "Array: copy overwrites destination prefix" prop_arrayCopy,
        runQC "Array: forall iteration accumulates elements" prop_arrayForallSum,
        runQC "Null: null eq null is true" prop_nullEq,
        runQC "Array: reference semantics across dup mutation" prop_arrayRefSemantics,
        runQC "Bitwise: and on integers" prop_bitwiseAnd,
        runQC "Bitwise: or on integers" prop_bitwiseOr,
        runQC "Bitwise: xor on integers" prop_bitwiseXor,
        runQC "Mark: counttomark counts elements above mark" prop_countToMark,
        runQC "Mark: cleartomark removes elements down through mark" prop_clearToMark,
        runQC "Control: exec evaluates procedure block" prop_execBlock,
        runQC "Control: exec leaves literal integer on stack" prop_execLiteral,
        runQC "Control: loop executes repeatedly until exit" prop_loopExit,
        runQC "Control: repeat unwinds cleanly on exit" prop_repeatExit,
        runQC "Control: for unwinds cleanly on exit" prop_forExit,
        runQC "Control: bind converts builtin operator names to operator objects" prop_bindOperator,
        runQC "Control: bind preserves literal integers inside procedures" prop_bindPreservesLiterals,
        runQC "Control: bind procedure executes correctly with bound operator" prop_bindExecutes,
        runQC "Transform: matrix allocates 6-element identity array" prop_matrixAllocLength,
        runQC "Transform: identmatrix fills array with identity matrix" prop_identmatrixIdentity,
        runQC "Transform: defaultmatrix fills array with default identity matrix" prop_defaultmatrixIdentity,
        runQC "Transform: translate with matrix argument builds translation matrix" prop_translateMatrix,
        runQC "Transform: scale with matrix argument builds scaling matrix" prop_scaleMatrix,
        runQC "Transform: rotate with matrix argument builds rotation matrix" prop_rotateMatrix,
        runQC "Transform: rotate CTM by 90 degrees rotates coordinate axis" prop_rotateCTM,
        runQC "Transform: concat premultiplies matrix onto CTM" prop_concatCTM,
        runQC "Transform: concatmatrix of two identity matrices produces identity" prop_concatmatrixIdentity,
        runQC "Transform: invertmatrix of identity is identity" prop_invertMatrixIdentity,
        runQC "Transform: dtransform ignores translation component of CTM" prop_dtransformIgnoresTranslation,
        runQC "Transform: dtransform and idtransform roundtrip distance vector" prop_dtransformIdtransformRoundTrip,
        runQC "Transform: currentmatrix, initmatrix, setmatrix restore CTM" prop_currentmatrixSetmatrix,
        runQC "Transform: transform and itransform roundtrip cleanly" prop_transformItransformRoundTrip,
        runQC "Path: moveto sets currentpoint" prop_movetoCurrentpoint,
        runQC "Path: rmoveto displaces currentpoint" prop_rmovetoRel,
        runQC "Path: lineto connects and updates currentpoint" prop_linetoCurrentpoint,
        runQC "Path: rlineto connects relatively and updates currentpoint" prop_rlinetoRel,
        runQC "Path: closepath closes subpath back to moveto start" prop_closepathReturnsToStart,
        runQC "Path: newpath clears path causing nocurrentpoint" prop_newpathClearsCurrentpoint,
        runQC "Path: curveto adds bezier and updates currentpoint to endpoint" prop_curvetoEndpoint,
        runQC "Path: rcurveto adds relative bezier and updates currentpoint" prop_rcurvetoEndpoint,
        runQC "Path: arc appends counter-clockwise circular arc to endpoint" prop_arcEndpoint,
        runQC "Path: arcn appends clockwise circular arc to endpoint" prop_arcnEndpoint,
        runQC "Path: arcto computes tangent arc points and adds to path" prop_arctoTangents,
        runQC "Path: pathbbox calculates user-space bounding box of path" prop_pathbboxRect,
        runQC "Path: flattenpath preserves path bounding box" prop_flattenpathPreservesBBox,
        runQC "Path: pathforall enumerates all path segments with procs" prop_pathforallAccumulates,
        runQC "Path: currentpoint inverse projects through modified CTM" prop_currentpointWithCTM,
        runQC "State: gsave and grestore preserve and restore color" prop_gsaveGrestoreRGB,
        runQC "State: grestoreall restores initial graphics state" prop_grestoreall,
        runQC "State: setrgbcolor and currentrgbcolor roundtrip" prop_setrgbcolor,
        runQC "State: setgray and currentgray calculate luminance" prop_setgrayCurrentgray,
        runQC "State: setcmykcolor converts CMYK to RGB" prop_setcmykcolor,
        runQC "State: sethsbcolor converts HSB to RGB" prop_sethsbcolor,
        runQC "State: setlinewidth and currentlinewidth roundtrip" prop_setlinewidth,
        runQC "State: stroke clears active path" prop_strokeClearsPath,
        runQC "State: fill clears active path" prop_fillClearsPath,
        runQC "State: eofill clears active path" prop_eofillClearsPath,
        runQC "State: clip preserves active path and currentpoint" prop_clipPreservesCurrentpoint,
        runQC "State: showpage clears active path" prop_showpageClearsPath,
        runQC "State: stroke emits valid SVG stroke element" prop_strokeEmitsSVG,
        runQC "State: fill emits valid SVG fill element" prop_fillEmitsSVG,
        runQC "State: eofill emits valid SVG evenodd fill element" prop_eofillEmitsSVG,
        runQC "Util: cvi truncates real numbers and parses string integers" prop_cviTruncatesReal,
        runQC "Util: rand generates non-negative pseudo-random integers" prop_randGeneratesPositiveInt,
        runQC "Util: aload unpacks array elements onto stack" prop_aloadUnpacksArray,
        runQC "Util: cvx converts literal array into executable block" prop_cvxExecutesBlock,
        runQC "Util: load successfully resolves and aliases built-in operators" prop_loadAliasesBuiltin,
        runQC "Util: where searches dictionary stack correctly" prop_whereFindsDict,
        runQC "Util: save and restore preserve graphics state" prop_saveRestorePreservesState,
        runQC "Util: clippath sets path to page boundary rectangle" prop_clippathEmitsBoundary,
        runQC "Util: bitshift shifts integer bits left and right" prop_bitshiftShiftsBits,
        runQC "Util: astore stores stack values into array" prop_astorePopsIntoArray,
        runQC "Util: cleardictstack resets dictionary stack to root" prop_cleardictstackResetsStack,
        runQC "Util: usertime returns non-negative execution time" prop_usertimeReturnsInt,
        runQC "Util: userdict pushes base dictionary" prop_userdictPushesRootDict,
        runQC "Util: trailing dot numbers parse as reals" prop_trailingDotParsesReal
      ]
  let allQcPassed = and qcResults

  putStrLn "\n--- Section 2: PostScript Program Integration Tests ---"
  programResults <- mapM runProgramTest testPrograms
  let allProgramsPassed = and programResults

  putStrLn "\n--- Section 3: Ghostscript Differential Tests (test/output_tests) ---"
  outputFiles <- listDirectory "test/output_tests"
  let diffFiles = sort ["test/output_tests" </> f | f <- outputFiles, ".ps" `isSuffixOf` f]
  diffResults <- mapM runDiffTest diffFiles
  let allDiffPassed = and diffResults

  if allQcPassed && allProgramsPassed && allDiffPassed
    then do
      putStrLn "\nRESULT: All QuickCheck properties, Program suites, and Ghostscript Differential tests PASSED!"
      exitSuccess
    else do
      putStrLn "\nRESULT: One or more tests FAILED!"
      exitFailure
