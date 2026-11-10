module PostScript.Primitives.Graphics.Transform
  ( psTranslate,
    psRotate,
    psScale,
    psConcat,
    psMatrix,
    psSetmatrix,
    psCurrentmatrix,
    psDefaultmatrix,
    psIdentmatrix,
    psConcatmatrix,
    psInvertmatrix,
    psTransform,
    psItransform,
    psDtransform,
    psIdtransform,
    psInitmatrix,
    multiplyMatrix,
    transformPoint,
    invertMatrix,
  )
where

import Control.Monad.State.Strict (gets, modify)
import Data.Map.Strict qualified as Map
import PostScript.Environment (modifyGState, newArray, peek, pop, push)
import PostScript.Types
  ( GraphicsState (..),
    Interpreter,
    Matrix,
    Object (..),
    PSState (..),
    identityMatrix,
  )

-- Number conversion helpers
toDouble :: Object -> Maybe Double
toDouble (PSInteger n) = Just (fromIntegral n)
toDouble (PSReal r) = Just r
toDouble _ = Nothing

popNum :: Interpreter Double
popNum =
  pop >>= \case
    PSInteger n -> return (fromIntegral n)
    PSReal r -> return r
    _ -> error "typecheck: expected number"

matrixToObjects :: Matrix -> [Object]
matrixToObjects (a, b, c, d, tx, ty) =
  [PSReal a, PSReal b, PSReal c, PSReal d, PSReal tx, PSReal ty]

objectsToMatrix :: [Object] -> Maybe Matrix
objectsToMatrix [o1, o2, o3, o4, o5, o6] = do
  a <- toDouble o1
  b <- toDouble o2
  c <- toDouble o3
  d <- toDouble o4
  tx <- toDouble o5
  ty <- toDouble o6
  Just (a, b, c, d, tx, ty)
objectsToMatrix _ = Nothing

popMatrix :: Interpreter (Int, Matrix)
popMatrix = do
  obj <- pop
  case obj of
    PSArray aid -> do
      heap <- gets arrayHeap
      case Map.lookup aid heap of
        Just elems -> case objectsToMatrix elems of
          Just m -> return (aid, m)
          Nothing -> error "typecheck: array is not a 6-element numeric matrix"
        Nothing -> error "undefined: array not found on heap"
    _ -> error "typecheck: expected matrix array"

popDestArray :: Interpreter Int
popDestArray =
  pop >>= \case
    PSArray aid -> do
      heap <- gets arrayHeap
      case Map.lookup aid heap of
        Just elems
          | length elems >= 6 -> return aid
          | otherwise -> error "rangecheck: matrix array must have length >= 6"
        Nothing -> error "undefined: array not found on heap"
    _ -> error "typecheck: expected matrix array"

writeMatrixArray :: Int -> Matrix -> Interpreter ()
writeMatrixArray aid m =
  modify (\s -> s {arrayHeap = Map.insert aid (matrixToObjects m) (arrayHeap s)})

-- 'matrix': -> [1 0 0 1 0 0] (push new identity matrix array)
psMatrix :: Interpreter ()
psMatrix = newArray (matrixToObjects identityMatrix) >>= push

-- 'initmatrix': -> - (reset CTM to identity)
psInitmatrix :: Interpreter ()
psInitmatrix = modifyGState (\gs -> gs {ctm = identityMatrix})

-- 'defaultmatrix': matrix -> matrix
psDefaultmatrix :: Interpreter ()
psDefaultmatrix = do
  aid <- popDestArray
  writeMatrixArray aid identityMatrix
  push (PSArray aid)

-- 'identmatrix': matrix -> matrix
psIdentmatrix :: Interpreter ()
psIdentmatrix = do
  aid <- popDestArray
  writeMatrixArray aid identityMatrix
  push (PSArray aid)

-- 'currentmatrix': matrix -> matrix
psCurrentmatrix :: Interpreter ()
psCurrentmatrix = do
  aid <- popDestArray
  m <- gets (ctm . gState)
  writeMatrixArray aid m
  push (PSArray aid)

-- 'setmatrix': matrix -> -
psSetmatrix :: Interpreter ()
psSetmatrix = do
  (_, m) <- popMatrix
  modifyGState (\gs -> gs {ctm = m})

-- 'concat': matrix -> -
psConcat :: Interpreter ()
psConcat = do
  (_, m) <- popMatrix
  modifyGState (\gs -> gs {ctm = multiplyMatrix m (ctm gs)})

-- 'concatmatrix': m1 m2 m3 -> m3 (m3 = m1 * m2)
psConcatmatrix :: Interpreter ()
psConcatmatrix = do
  aid3 <- popDestArray
  (_, m2) <- popMatrix
  (_, m1) <- popMatrix
  let mRes = multiplyMatrix m1 m2
  writeMatrixArray aid3 mRes
  push (PSArray aid3)

-- 'invertmatrix': m1 m2 -> m2
psInvertmatrix :: Interpreter ()
psInvertmatrix = do
  aid2 <- popDestArray
  (_, m1) <- popMatrix
  case invertMatrix m1 of
    Just mInv -> do
      writeMatrixArray aid2 mInv
      push (PSArray aid2)
    Nothing -> error "undefinedresult: singular matrix in invertmatrix"

-- 'translate': tx ty -> - OR tx ty matrix -> matrix
psTranslate :: Interpreter ()
psTranslate = do
  top <- peek
  case top of
    PSArray _ -> do
      aid <- popDestArray
      ty <- popNum
      tx <- popNum
      let t = (1.0, 0.0, 0.0, 1.0, tx, ty)
      writeMatrixArray aid t
      push (PSArray aid)
    _ -> do
      ty <- popNum
      tx <- popNum
      let t = (1.0, 0.0, 0.0, 1.0, tx, ty)
      modifyGState (\gs -> gs {ctm = multiplyMatrix t (ctm gs)})

-- 'scale': sx sy -> - OR sx sy matrix -> matrix
psScale :: Interpreter ()
psScale = do
  top <- peek
  case top of
    PSArray _ -> do
      aid <- popDestArray
      sy <- popNum
      sx <- popNum
      let s = (sx, 0.0, 0.0, sy, 0.0, 0.0)
      writeMatrixArray aid s
      push (PSArray aid)
    _ -> do
      sy <- popNum
      sx <- popNum
      let s = (sx, 0.0, 0.0, sy, 0.0, 0.0)
      modifyGState (\gs -> gs {ctm = multiplyMatrix s (ctm gs)})

-- 'rotate': angle -> - OR angle matrix -> matrix
psRotate :: Interpreter ()
psRotate = do
  top <- peek
  case top of
    PSArray _ -> do
      aid <- popDestArray
      deg <- popNum
      let rad = deg * pi / 180.0
          c = cos rad
          s = sin rad
          r = (c, s, -s, c, 0.0, 0.0)
      writeMatrixArray aid r
      push (PSArray aid)
    _ -> do
      deg <- popNum
      let rad = deg * pi / 180.0
          c = cos rad
          s = sin rad
          r = (c, s, -s, c, 0.0, 0.0)
      modifyGState (\gs -> gs {ctm = multiplyMatrix r (ctm gs)})

-- Helper to pop point and optional matrix for projection operators
popPointWithMatrix :: Interpreter ((Double, Double), Matrix)
popPointWithMatrix = do
  top <- peek
  case top of
    PSArray _ -> do
      (_, m) <- popMatrix
      y <- popNum
      x <- popNum
      return ((x, y), m)
    _ -> do
      m <- gets (ctm . gState)
      y <- popNum
      x <- popNum
      return ((x, y), m)

-- 'transform': x y -> x' y' OR x y matrix -> x' y'
psTransform :: Interpreter ()
psTransform = do
  (pt, m) <- popPointWithMatrix
  let (x', y') = transformPoint m pt
  push (PSReal x') >> push (PSReal y')

-- 'itransform': x' y' -> x y OR x' y' matrix -> x y
psItransform :: Interpreter ()
psItransform = do
  (pt, m) <- popPointWithMatrix
  case invertMatrix m of
    Just mInv -> do
      let (x, y) = transformPoint mInv pt
      push (PSReal x) >> push (PSReal y)
    Nothing -> error "undefinedresult: singular matrix in itransform"

-- 'dtransform': dx dy -> dx' dy' OR dx dy matrix -> dx' dy'
psDtransform :: Interpreter ()
psDtransform = do
  ((dx, dy), (a, b, c, d, _, _)) <- popPointWithMatrix
  let dx' = a * dx + c * dy
      dy' = b * dx + d * dy
  push (PSReal dx') >> push (PSReal dy')

-- 'idtransform': dx' dy' -> dx dy OR dx' dy' matrix -> dx dy
psIdtransform :: Interpreter ()
psIdtransform = do
  ((dx, dy), m) <- popPointWithMatrix
  case invertMatrix m of
    Just (a, b, c, d, _, _) -> do
      let dx' = a * dx + c * dy
          dy' = b * dx + d * dy
      push (PSReal dx') >> push (PSReal dy')
    Nothing -> error "undefinedresult: singular matrix in idtransform"

-- Helpers for affine matrix math
multiplyMatrix :: Matrix -> Matrix -> Matrix
multiplyMatrix (a1, b1, c1, d1, tx1, ty1) (a2, b2, c2, d2, tx2, ty2) =
  ( a1 * a2 + b1 * c2,
    a1 * b2 + b1 * d2,
    c1 * a2 + d1 * c2,
    c1 * b2 + d1 * d2,
    tx1 * a2 + ty1 * c2 + tx2,
    tx1 * b2 + ty1 * d2 + ty2
  )

transformPoint :: Matrix -> (Double, Double) -> (Double, Double)
transformPoint (a, b, c, d, tx, ty) (x, y) =
  (a * x + c * y + tx, b * x + d * y + ty)

invertMatrix :: Matrix -> Maybe Matrix
invertMatrix (a, b, c, d, tx, ty)
  | abs det < 1e-12 = Nothing
  | otherwise =
      Just
        ( d / det,
          (-b) / det,
          (-c) / det,
          a / det,
          (c * ty - d * tx) / det,
          (b * tx - a * ty) / det
        )
  where
    det = a * d - b * c
