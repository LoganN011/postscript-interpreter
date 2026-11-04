module PostScript.Primitives.Graphics.Transform where

import PostScript.Types (Interpreter, Matrix)

-- Matrix transformation primitives

-- 'translate': tx ty translate -> apply translation to CTM
psTranslate :: Interpreter ()
psTranslate = undefined

-- 'rotate': angle rotate -> apply rotation to CTM (degrees)
psRotate :: Interpreter ()
psRotate = undefined

-- 'scale': sx sy scale -> apply scaling to CTM
psScale :: Interpreter ()
psScale = undefined

-- 'concat': matrix concat -> multiply CTM by 6-element matrix array
psConcat :: Interpreter ()
psConcat = undefined

-- 'matrix': -> [1 0 0 1 0 0] (push identity matrix array)
psMatrix :: Interpreter ()
psMatrix = undefined

-- 'setmatrix': matrix setmatrix -> replace CTM with matrix from stack
psSetmatrix :: Interpreter ()
psSetmatrix = undefined

-- Coordinate projection & matrix operations

-- 'transform': x y transform -> x' y' (project point via CTM)
psTransform :: Interpreter ()
psTransform = undefined

-- 'itransform': x' y' itransform -> x y (inverse project via CTM)
psItransform :: Interpreter ()
psItransform = undefined

-- 'dtransform': dx dy dtransform -> dx' dy' (transform distance vector)
psDtransform :: Interpreter ()
psDtransform = undefined

-- 'idtransform': dx' dy' idtransform -> dx dy (inverse transform distance vector)
psIdtransform :: Interpreter ()
psIdtransform = undefined

-- 'currentmatrix': matrix currentmatrix -> copy CTM into matrix
psCurrentmatrix :: Interpreter ()
psCurrentmatrix = undefined

-- 'defaultmatrix': matrix defaultmatrix -> copy default matrix into matrix
psDefaultmatrix :: Interpreter ()
psDefaultmatrix = undefined

-- 'identmatrix': matrix identmatrix -> reset matrix to identity
psIdentmatrix :: Interpreter ()
psIdentmatrix = undefined

-- 'concatmatrix': m1 m2 m3 concatmatrix -> m3 = m1 * m2
psConcatmatrix :: Interpreter ()
psConcatmatrix = undefined

-- 'invertmatrix': m1 m2 invertmatrix -> m2 = invert(m1)
psInvertmatrix :: Interpreter ()
psInvertmatrix = undefined

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
