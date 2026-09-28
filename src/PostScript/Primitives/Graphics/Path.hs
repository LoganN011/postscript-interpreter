{- ============================================================================
   MODULE: PostScript.Primitives.Graphics.Path
   PATH:   src/PostScript/Primitives/Graphics/Path.hs

   DESCRIPTION:
     Implements PostScript path construction primitives. Manipulates vector path 
     segments in the active `GraphicsState`.

   RESPONSIBILITIES:
     1. Path Construction:
        - `newpath`: Clear `currentPath` and reset `currentPoint` in active `GraphicsState`.
        - `moveto`: Set absolute position `(x, y)` transformed by CTM, add `MoveTo`.
        - `rmoveto`: Relative move `(dx, dy)` from `currentPoint`.
        - `lineto`: Add line segment from `currentPoint` to `(x, y)` transformed by CTM.
        - `rlineto`: Relative line `(dx, dy)` from `currentPoint`.
        - `arc`: Add arc segment `(x, y, radius, angle1, angle2)` counter-clockwise.
        - `arcn`: Add arc segment clockwise.
        - `closepath`: Connect current point back to start of path with `ClosePath`.

   FUNCTIONS TO IMPLEMENT:
     - psNewpath :: Interpreter ()
     - psMoveto, psRmoveto :: Interpreter ()
     - psLineto, psRlineto :: Interpreter ()
     - psArc, psArcn :: Interpreter ()
     - psClosepath :: Interpreter ()
   ============================================================================ -}