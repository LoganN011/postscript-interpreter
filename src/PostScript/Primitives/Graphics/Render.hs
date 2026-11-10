module PostScript.Primitives.Graphics.Render
  ( renderPathToSVG,
    renderStrokeToSVG,
    renderFillToSVG,
    renderTextToSVG,
    generateSVGDocument,
  )
where

import Data.List (intercalate)
import PostScript.Types (GraphicsState (..), PathSegment (..))
import Text.Printf (printf)

-- Format number cleanly without unnecessary trailing decimals
fmtD :: Double -> String
fmtD d
  | fromIntegral (round d :: Int) == d = show (round d :: Int)
  | otherwise = printf "%.3f" d

-- Convert path segments to SVG path data d="..."
renderPathToSVG :: [PathSegment] -> String
renderPathToSVG segs = unwords (map segToSVG segs)
  where
    segToSVG (MoveTo x y) = "M " ++ fmtD x ++ " " ++ fmtD y
    segToSVG (LineTo x y) = "L " ++ fmtD x ++ " " ++ fmtD y
    segToSVG (CurveTo x1 y1 x2 y2 x3 y3) =
      "C " ++ unwords (map fmtD [x1, y1, x2, y2, x3, y3])
    segToSVG (Arc cx cy r a1 a2) =
      let rad2 = a2 * pi / 180.0
          x2 = cx + r * cos rad2
          y2 = cy + r * sin rad2
          angSpan = if a2 <= a1 then a2 - a1 + 360.0 else a2 - a1
          largeArc = if angSpan > 180.0 then "1" else "0"
          sweep = "1"
       in "A " ++ fmtD r ++ " " ++ fmtD r ++ " 0 " ++ largeArc ++ " " ++ sweep ++ " " ++ fmtD x2 ++ " " ++ fmtD y2
    segToSVG (ArcN cx cy r a1 a2) =
      let rad2 = a2 * pi / 180.0
          x2 = cx + r * cos rad2
          y2 = cy + r * sin rad2
          angSpan = if a1 <= a2 then a1 - a2 + 360.0 else a1 - a2
          largeArc = if angSpan > 180.0 then "1" else "0"
          sweep = "0"
       in "A " ++ fmtD r ++ " " ++ fmtD r ++ " 0 " ++ largeArc ++ " " ++ sweep ++ " " ++ fmtD x2 ++ " " ++ fmtD y2
    segToSVG ClosePath = "Z"

-- Format RGB color as "rgb(r, g, b)"
fmtRGB :: (Double, Double, Double) -> String
fmtRGB (r, g, b) =
  let clamp255 v = round (max 0.0 (min 1.0 v) * 255.0) :: Int
   in printf "rgb(%d,%d,%d)" (clamp255 r) (clamp255 g) (clamp255 b)

-- Format stroke attributes (color, width, dash, cap, join)
renderStrokeToSVG :: GraphicsState -> [PathSegment] -> String
renderStrokeToSVG gs segs =
  let dAttr = renderPathToSVG segs
      color = fmtRGB (rgbColor gs)
      w = fmtD (lineWidth gs)
      cap = case lineCap gs of
        1 -> "round"
        2 -> "square"
        _ -> "butt"
      join = case lineJoin gs of
        1 -> "round"
        2 -> "bevel"
        _ -> "miter"
      miterAttr =
        if lineJoin gs == 0 && miterLimit gs /= 4.0
          then " stroke-miterlimit=\"" ++ fmtD (miterLimit gs) ++ "\""
          else ""
      dashAttr =
        if null (dashArray gs)
          then ""
          else " stroke-dasharray=\"" ++ intercalate "," (map fmtD (dashArray gs)) ++ "\""
      dashOffsetAttr =
        if dashOffset gs == 0.0
          then ""
          else " stroke-dashoffset=\"" ++ fmtD (dashOffset gs) ++ "\""
   in "<path d=\""
        ++ dAttr
        ++ "\" fill=\"none\" stroke=\""
        ++ color
        ++ "\" stroke-width=\""
        ++ w
        ++ "\" stroke-linecap=\""
        ++ cap
        ++ "\" stroke-linejoin=\""
        ++ join
        ++ "\""
        ++ miterAttr
        ++ dashAttr
        ++ dashOffsetAttr
        ++ " />"

-- Format fill attributes (color, fill-rule)
renderFillToSVG :: GraphicsState -> Bool -> [PathSegment] -> String
renderFillToSVG gs isEvenOdd segs =
  let dAttr = renderPathToSVG segs
      color = fmtRGB (rgbColor gs)
      ruleAttr = if isEvenOdd then " fill-rule=\"evenodd\"" else ""
   in "<path d=\"" ++ dAttr ++ "\" fill=\"" ++ color ++ "\"" ++ ruleAttr ++ " stroke=\"none\" />"

-- Escape text for XML/SVG
escapeXML :: String -> String
escapeXML [] = []
escapeXML ('&' : cs) = "&amp;" ++ escapeXML cs
escapeXML ('<' : cs) = "&lt;" ++ escapeXML cs
escapeXML ('>' : cs) = "&gt;" ++ escapeXML cs
escapeXML ('"' : cs) = "&quot;" ++ escapeXML cs
escapeXML ('\'' : cs) = "&apos;" ++ escapeXML cs
escapeXML (c : cs) = c : escapeXML cs

-- Format scalable SVG text element
renderTextToSVG :: GraphicsState -> (Double, Double) -> String -> String
renderTextToSVG gs (x, y) text =
  let color = fmtRGB (rgbColor gs)
      fName = fontName gs
      fSize = fmtD (fontSize gs)
   in "<text x=\""
        ++ fmtD x
        ++ "\" y=\""
        ++ fmtD y
        ++ "\" font-family=\""
        ++ fName
        ++ "\" font-size=\""
        ++ fSize
        ++ "\" fill=\""
        ++ color
        ++ "\">"
        ++ escapeXML text
        ++ "</text>"

-- Wrap canvas elements in root SVG tag with Y-axis inversion
generateSVGDocument :: Double -> Double -> [String] -> String
generateSVGDocument w h elems =
  unlines
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
      "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\""
        ++ fmtD w
        ++ "pt\" height=\""
        ++ fmtD h
        ++ "pt\" viewBox=\"0 0 "
        ++ fmtD w
        ++ " "
        ++ fmtD h
        ++ "\">",
      "  <g transform=\"translate(0, " ++ fmtD h ++ ") scale(1, -1)\">",
      unlines (map ("    " ++) elems),
      "  </g>",
      "</svg>"
    ]
