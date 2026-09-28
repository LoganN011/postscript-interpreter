{- ============================================================================
   MODULE: PostScript.Primitives.Graphics.Transform
   PATH:   src/PostScript/Primitives/Graphics/Transform.hs

   DESCRIPTION:
     Implements coordinate system transformation primitives by modifying 
     the Current Transformation Matrix (CTM) in the active `GraphicsState`.

   RESPONSIBILITIES:
     1. Matrix Operators:
        - `translate`: Pop `tx`, `ty`. Multiply CTM by translation matrix.
        - `rotate`: Pop angle $\theta$ (degrees). Multiply CTM by rotation matrix.
        - `scale`: Pop `sx`, `sy`. Multiply CTM by scaling matrix.
        - `concat`: Pop matrix array, multiply CTM by given matrix.
        - `matrix`: Push identity matrix array `[1 0 0 1 0 0]`.
        - `setmatrix`: Replace current CTM with matrix from stack.

   FUNCTIONS TO IMPLEMENT:
     - psTranslate :: Interpreter ()
     - psRotate :: Interpreter ()
     - psScale :: Interpreter ()
     - psConcat :: Interpreter ()
     - psMatrix :: Interpreter ()
     - psSetmatrix :: Interpreter ()
     - multiplyMatrix :: Matrix -> Matrix -> Matrix
         Helper function to perform $2 \times 3$ affine matrix multiplication.
   ============================================================================ -}