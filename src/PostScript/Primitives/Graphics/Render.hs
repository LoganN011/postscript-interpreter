{- ============================================================================
   MODULE: PostScript.Primitives.Graphics.Render
   PATH:   src/PostScript/Primitives/Graphics/Render.hs

   DESCRIPTION:
     Pure vector rasterizer/emitter backend. Takes accumulated `PathSegment` items 
     and `GraphicsState` attributes, transforms coordinates through the CTM, 
     and constructs valid SVG XML markup string output.

   RESPONSIBILITIES:
     1. Coordinate Transformation:
        - Apply CTM affine transformation to `(x, y)` points before rendering to SVG space.
        - Handle SVG coordinate system inversion (PostScript origin is bottom-left; SVG is top-left).
     2. SVG Vector String Generation:
        - Convert `[PathSegment]` list into SVG `<path d="..." />` string commands.
        - Convert `MoveTo` -> `M x y`.
        - Convert `LineTo` -> `L x y`.
        - Convert `Arc` -> SVG elliptical arc command `A rx ry x-axis-rotation large-arc-flag sweep-flag x y`.
        - Convert `ClosePath` -> `Z`.
     3. Document Assembly:
        - Format fill color, stroke color (`rgb(...)`), and line width attributes into XML nodes.
        - Wrap full vector output in standard `<svg width="..." height="..." ...> </svg>` XML root tag.

   FUNCTIONS TO IMPLEMENT:
     - renderPathToSVG :: GraphicsState -> String
         Translates `currentPath` into SVG `<path d="..." />` element.
     - transformPoint :: Matrix -> (Double, Double) -> (Double, Double)
         Applies affine matrix math to a 2D coordinate point.
     - generateSVGDocument :: [String] -> String
         Wraps generated SVG elements into full valid XML string.
   ============================================================================ -}