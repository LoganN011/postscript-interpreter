{- ============================================================================
   MODULE: PostScript.Primitives.Graphics.State
   PATH:   src/PostScript/Primitives/Graphics/State.hs

   DESCRIPTION:
     Implements graphics state attribute management operators (`gsave`, `grestore`,
     colors, line widths) and triggers vector rendering actions.

   RESPONSIBILITIES:
     1. Graphics State Stack:
        - `gsave`: Push duplicate of current `GraphicsState` onto `gStateStack`.
        - `grestore`: Pop top `GraphicsState` off `gStateStack` and restore it.
     2. Color & Style Attributes:
        - `setrgbcolor`: Pop $r, g, b$ ($0.0 \dots 1.0$), set `rgbColor` in `GraphicsState`.
        - `setgray`: Pop gray level ($0.0 \dots 1.0$), map to $r=g=b$, set `rgbColor`.
        - `setlinewidth`: Pop number, set `lineWidth` in `GraphicsState`.
     3. Painting Primitives:
        - `stroke`: Render outlines of `currentPath` using active stroke attributes, then call `newpath`.
        - `fill`: Fill interior of `currentPath` with active color, then call `newpath`.
        - `showpage`: Flush active canvas state to SVG representation and reset paths.

   FUNCTIONS TO IMPLEMENT:
     - psGsave :: Interpreter ()
     - psGrestore :: Interpreter ()
     - psSetrgbcolor, psSetgray :: Interpreter ()
     - psSetlinewidth :: Interpreter ()
     - psStroke :: Interpreter ()
     - psFill :: Interpreter ()
     - psShowpage :: Interpreter ()
   ============================================================================ -}