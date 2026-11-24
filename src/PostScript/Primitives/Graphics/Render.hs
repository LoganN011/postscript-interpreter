module PostScript.Primitives.Graphics.Render
  ( renderPathToSVG,
    renderStrokeToSVG,
    renderFillToSVG,
    renderTextToSVG,
    generateSVGDocument,
    renderCommandToSVG,
    splitByPages,
    renderAllPagesToSVG,
  )
where

import Data.List (foldl', isInfixOf)
import PostScript.Types (GraphicCommand (..), GraphicsState (..), PathSegment (..))

-- Format number cleanly without unnecessary trailing decimals
fmtD :: Double -> String
fmtD d
  | fromIntegral (round d :: Int) == d = show (round d :: Int)
  | otherwise =
      let rounded = fromIntegral (round (d * 1000.0) :: Int) / 1000.0 :: Double
       in show rounded

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
  let c255 v = show (round (max 0.0 (min 1.0 v) * 255.0) :: Int)
   in "rgb(" ++ c255 r ++ "," ++ c255 g ++ "," ++ c255 b ++ ")"

-- Comma separated list of strings
joinComma :: [String] -> String
joinComma [] = ""
joinComma [x] = x
joinComma (x : xs) = x ++ "," ++ joinComma xs

-- Deterministic hash for SVG clipPath IDs
hashString :: String -> Int
hashString = foldl' (\h ch -> (h * 31 + fromEnum ch) `mod` 1000000007) 5381

-- Wrap an SVG element with a clipPath when GraphicsState has an active clipPath
wrapWithClip :: GraphicsState -> String -> String
wrapWithClip gs elemStr =
  case clipPath gs of
    Just cSegs
      | not (null cSegs) ->
          let dClip = renderPathToSVG cSegs
              cid = "clip_" ++ show (abs (hashString dClip))
           in "<g><clipPath id=\""
                ++ cid
                ++ "\"><path d=\""
                ++ dClip
                ++ "\" /></clipPath><g clip-path=\"url(#"
                ++ cid
                ++ ")\">"
                ++ elemStr
                ++ "</g></g>"
    _ -> elemStr

-- Format stroke attributes (color, width, dash, cap, join)
renderStrokeToSVG :: GraphicsState -> [PathSegment] -> String
renderStrokeToSVG gs segs =
  let dAttr = renderPathToSVG segs
      color = fmtRGB (rgbColor gs)
      (a, b, c, d, _, _) = ctm gs
      detScale = sqrt (abs (a * d - b * c))
      scaleFactor = if detScale == 0 then 1.0 else detScale
      w = fmtD (if lineWidth gs <= 0 then 1.0 else lineWidth gs * scaleFactor)
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
      effDashes = map (* scaleFactor) (dashArray gs)
      effOffset = dashOffset gs * scaleFactor
      dashAttr =
        if null effDashes
          then ""
          else " stroke-dasharray=\"" ++ joinComma (map fmtD effDashes) ++ "\""
      dashOffsetAttr =
        if effOffset == 0.0
          then ""
          else " stroke-dashoffset=\"" ++ fmtD effOffset ++ "\""
   in wrapWithClip gs $
        "<path d=\""
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
   in wrapWithClip gs $
        "<path d=\"" ++ dAttr ++ "\" fill=\"" ++ color ++ "\"" ++ ruleAttr ++ " stroke=\"none\" />"

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
      rawSize = fontSize gs
      fSize = abs rawSize
      sgn = if rawSize < 0 then -1.0 else 1.0
      (a, b, c, d, _, _) = ctm gs
      transformAttr =
        " transform=\"matrix("
          ++ fmtD (a * sgn)
          ++ ", "
          ++ fmtD (b * sgn)
          ++ ", "
          ++ fmtD (-c * sgn)
          ++ ", "
          ++ fmtD (-d * sgn)
          ++ ", "
          ++ fmtD x
          ++ ", "
          ++ fmtD y
          ++ ")\""
      styleAttr =
        if "Italic" `isInfixOf` fName || "Oblique" `isInfixOf` fName
          then " font-style=\"italic\""
          else ""
      weightAttr =
        if "Bold" `isInfixOf` fName
          then " font-weight=\"bold\""
          else ""
   in wrapWithClip gs $
        "<text x=\"0\" y=\"0\""
          ++ transformAttr
          ++ " font-family=\""
          ++ escapeXML fName
          ++ "\" font-size=\""
          ++ fmtD fSize
          ++ "\""
          ++ styleAttr
          ++ weightAttr
          ++ " fill=\""
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

-- Convert a GraphicCommand into an SVG string element, ignoring page boundaries
renderCommandToSVG :: GraphicCommand -> Maybe String
renderCommandToSVG (DrawStroke gs segs) = Just (renderStrokeToSVG gs segs)
renderCommandToSVG (DrawFill gs isEvenOdd segs) = Just (renderFillToSVG gs isEvenOdd segs)
renderCommandToSVG (DrawText gs pt txt) = Just (renderTextToSVG gs pt txt)
renderCommandToSVG DrawPage = Nothing

-- Partition GraphicCommands into pages delimited by DrawPage
splitByPages :: [GraphicCommand] -> [[GraphicCommand]]
splitByPages = go []
  where
    go curr [] = if null curr then [] else [reverse curr]
    go curr (DrawPage : rest) =
      let p = reverse curr
          restPages = go [] rest
       in if null p then restPages else p : restPages
    go curr (c : rest) = go (c : curr) rest

-- Render all pages in GraphicCommands to separate SVG documents
renderAllPagesToSVG :: Double -> Double -> [GraphicCommand] -> [String]
renderAllPagesToSVG w h cmds =
  let pages = splitByPages cmds
      renderOnePage pCmds =
        let elems = [s | c <- pCmds, Just s <- [renderCommandToSVG c]]
         in generateSVGDocument w h elems
   in map renderOnePage pages
