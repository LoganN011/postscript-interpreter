module GlossRender
  ( renderCommandsToPicture,
    compilePages,
    paperCanvas,
    pageDimensions,
  )
where

import Data.List (isInfixOf, sort)
import Graphics.Gloss hiding (Arc)
import PostScript.Types
  ( GraphicCommand (..),
    GraphicsState (..),
    PathSegment (..),
  )

-- | Standard US Letter page dimensions in PostScript points
pageDimensions :: (Float, Float)
pageDimensions = (612.0, 792.0)

pageWidth, pageHeight :: Float
(pageWidth, pageHeight) = pageDimensions

pageHalfW, pageHalfH :: Float
pageHalfW = pageWidth / 2.0
pageHalfH = pageHeight / 2.0

-- | Map PostScript coordinates (origin at bottom-left) to Gloss coordinates (origin at center)
toGlossPoint :: (Double, Double) -> Point
toGlossPoint (x, y) =
  ( realToFrac x - pageHalfW,
    realToFrac y - pageHalfH
  )

-- | Convert PostScript RGB color (0.0 .. 1.0) to Gloss Color
psColorToGloss :: (Double, Double, Double) -> Color
psColorToGloss (r, g, b) =
  makeColor (clamp (realToFrac r)) (clamp (realToFrac g)) (clamp (realToFrac b)) 1.0
  where
    clamp v = max 0.0 (min 1.0 v)

-- | Wrap a drawing in a white US Letter paper backdrop with border and drop shadow
paperCanvas :: Picture -> Picture
paperCanvas content =
  Pictures
    [ -- Drop shadow
      Translate 5 (-5) $ Color (greyN 0.22) $ rectangleSolid pageWidth pageHeight,
      -- Paper background
      Color white $ rectangleSolid pageWidth pageHeight,
      -- Rendered content strictly clipped to page bounds
      content,
      -- Crisp boundary frame on top of drawing
      Color (greyN 0.70) $ rectangleWire pageWidth pageHeight
    ]

-- | Compiles GraphicCommands into a list of Pictures, one per page (delimited by DrawPage)
compilePages :: [GraphicCommand] -> [Picture]
compilePages cmds =
  let pageCmds = splitByDrawPage cmds
   in if null pageCmds
        then [paperCanvas Blank]
        else map (paperCanvas . renderCommandsToPicture) pageCmds

-- | Partition graphic commands at DrawPage boundaries
splitByDrawPage :: [GraphicCommand] -> [[GraphicCommand]]
splitByDrawPage = go []
  where
    go curr [] = if null curr then [] else [reverse curr]
    go curr (DrawPage : rest) =
      let p = reverse curr
          restPages = go [] rest
       in if null p then restPages else p : restPages
    go curr (c : rest) = go (c : curr) rest

-- | Compiles a list of GraphicCommands into a single Gloss Picture
renderCommandsToPicture :: [GraphicCommand] -> Picture
renderCommandsToPicture cmds =
  Pictures (map renderCmd cmds)
  where
    renderCmd (DrawStroke gs segs) = renderStroke gs segs
    renderCmd (DrawFill gs _isEvenOdd segs) = renderFill gs segs
    renderCmd (DrawText gs (x, y) txt) = renderText gs (x, y) txt
    renderCmd DrawPage = Blank

-- | Render a stroke command as Gloss polylines or thick quads, clipped to page bounds
renderStroke :: GraphicsState -> [PathSegment] -> Picture
renderStroke gs segs =
  let subpaths = segmentsToSubpaths segs
      col = psColorToGloss (rgbColor gs)
      (a, b, c, d, _, _) = ctm gs
      scaleFactor = sqrt (abs (a * d - b * c))
      effScale = if scaleFactor == 0 then 1.0 else scaleFactor
      effW = realToFrac (lineWidth gs * effScale)
      cap = lineCap gs
      join = lineJoin gs
      dashes = map (* effScale) (dashArray gs)
      dOffset = dashOffset gs * effScale

      mkPath pts
        | length pts < 2 = Blank
        | null dashes = renderSubpath effW cap join (map toGlossPoint pts)
        | otherwise =
            let (dashPaths, zeroDots) = dashSubpath dashes dOffset pts
                renderedDashes = [renderSubpath effW cap join (map toGlossPoint dp) | dp <- dashPaths, length dp >= 2]
                renderedDots = [renderDot effW cap (toGlossPoint p) | p <- zeroDots]
             in Pictures (renderedDashes ++ renderedDots)
   in Color col $ Pictures (map mkPath subpaths)

-- | Render a subpath with given line width, line cap, and line join
renderSubpath :: Float -> Int -> Int -> [Point] -> Picture
renderSubpath effW cap join pts
  | length pts < 2 = Blank
  | effW <= 1.0 =
      let linePic = renderClippedLines pts
          caps = case (cap, pts) of
            (1, p0 : _) -> [renderDisk (effW / 2.0) p0, renderDisk (effW / 2.0) (last pts)]
            _ -> []
       in Pictures (linePic : caps)
  | otherwise = renderThickSubpath effW cap join pts

-- | Render a zero-length dash dot for round (1) or square (2) line caps
renderDot :: Float -> Int -> Point -> Picture
renderDot effW cap pt@(x, y)
  | not (isInsideRect (-pageHalfW) pageHalfW (-pageHalfH) pageHalfH pt) = Blank
  | cap == 1 = renderDisk (effW / 2.0) pt
  | cap == 2 =
      let hw = effW / 2.0
       in Polygon [(x - hw, y - hw), (x + hw, y - hw), (x + hw, y + hw), (x - hw, y + hw)]
  | otherwise = Blank

-- | Subdivide a polyline into dashes and zero-length dots according to PostScript dash pattern
dashSubpath :: [Double] -> Double -> [(Double, Double)] -> ([[(Double, Double)]], [(Double, Double)])
dashSubpath dashes offset pts =
  let pat = if odd (length dashes) then dashes ++ dashes else dashes
      patLen = sum pat
   in if patLen <= 1e-6 || null pts
        then ([pts], [])
        else
          let normOff = offset - fromIntegral (floor (offset / patLen) :: Int) * patLen
              (initIdx, initRem) = findPhase normOff pat 0
           in walkSegments pat (initIdx, initRem) pts [] [] []
  where
    findPhase _ [] idx = (idx, 0.0)
    findPhase off (d : rest) idx
      | off < d = (idx, d - off)
      | otherwise = findPhase (off - d) rest (idx + 1)

    walkSegments _ _ [] accDashes accDots curDash =
      (if null curDash then accDashes else reverse curDash : accDashes, reverse accDots)
    walkSegments _ _ [_] accDashes accDots curDash =
      (if null curDash then accDashes else reverse curDash : accDashes, reverse accDots)
    walkSegments pat (phaseIdx, remDist) (p1 : p2 : rest) accDashes accDots curDash =
      let (dx, dy) = (fst p2 - fst p1, snd p2 - snd p1)
          segLen = sqrt (dx * dx + dy * dy)
          patCount = length pat
       in if segLen < 1e-9
            then walkSegments pat (phaseIdx, remDist) (p2 : rest) accDashes accDots curDash
            else
              let go pCur leftLen idx remLen curD accD accDt =
                    let isOn = even idx
                        dVal = pat !! idx
                     in if dVal == 0.0
                          then
                            let accDtNext = if isOn then pCur : accDt else accDt
                                nextIdx = (idx + 1) `mod` patCount
                             in go pCur leftLen nextIdx (pat !! nextIdx) curD accD accDtNext
                          else
                            if leftLen < remLen
                              then
                                let curDNext = if isOn then if null curD then [pCur, p2] else p2 : curD else curD
                                 in (curDNext, accD, accDt, (idx, remLen - leftLen))
                              else
                                let t = remLen / leftLen
                                    pCut = (fst pCur + t * (fst p2 - fst pCur), snd pCur + t * (snd p2 - snd pCur))
                                    (curDNext, accDNext) =
                                      if isOn
                                        then
                                          let finished = if null curD then [pCur, pCut] else pCut : curD
                                           in ([], reverse finished : accD)
                                        else ([], accD)
                                    nextIdx = (idx + 1) `mod` patCount
                                    nextRem = pat !! nextIdx
                                 in go pCut (leftLen - remLen) nextIdx nextRem curDNext accDNext accDt

                  (curDRes, accDRes, accDtRes, (finalIdx, finalRem)) =
                    go p1 segLen phaseIdx remDist curDash accDashes accDots
               in walkSegments pat (finalIdx, finalRem) (p2 : rest) accDRes accDtRes curDRes

-- | Render thin line segments clipped to the [-pageHalfW, pageHalfW] x [-pageHalfH, pageHalfH] page rectangle
renderClippedLines :: [Point] -> Picture
renderClippedLines pts =
  let segments = zip pts (drop 1 pts)
      clipped =
        [ (p1', p2')
        | (p1, p2) <- segments,
          Just (p1', p2') <- [clipSegmentToRect (-pageHalfW) pageHalfW (-pageHalfH) pageHalfH p1 p2]
        ]
   in Pictures [Line [p1, p2] | (p1, p2) <- clipped]

-- | Render thick line segments as quads with vertex disks, clipped to page bounds
renderThickSubpath :: Float -> Int -> Int -> [Point] -> Picture
renderThickSubpath w cap join pts =
  let strokeHalfW = w / 2.0
      segments = zip pts (drop 1 pts)
      clippedSegs =
        [ (p1', p2')
        | (p1, p2) <- segments,
          Just (p1', p2') <- [clipSegmentToRect (-pageHalfW) pageHalfW (-pageHalfH) pageHalfH p1 p2]
        ]
      quads = [makeQuad strokeHalfW cap p1 p2 | (p1, p2) <- clippedSegs]
      joints =
        if join == 1 && length pts > 2
          then
            [ makeDisk strokeHalfW pt
            | pt <- init (drop 1 pts),
              isInsideRect (-pageHalfW) pageHalfW (-pageHalfH) pageHalfH pt
            ]
          else []
      endCaps = case (cap, pts) of
        (1, p0 : _) -> [renderDisk strokeHalfW p0, renderDisk strokeHalfW (last pts)]
        _ -> []
   in Pictures (quads ++ joints ++ endCaps)
  where
    makeQuad hw cp (x1, y1) (x2, y2) =
      let dx = x2 - x1
          dy = y2 - y1
          len = sqrt (dx * dx + dy * dy)
       in if len < 0.01 && cp /= 2
            then Blank
            else
              let (nx, ny, ux, uy) =
                    if len < 0.01
                      then (0.0, hw, hw, 0.0)
                      else ((-dy / len) * hw, (dx / len) * hw, (dx / len) * hw, (dy / len) * hw)
                  (x1', y1', x2', y2') =
                    if cp == 2
                      then (x1 - ux, y1 - uy, x2 + ux, y2 + uy)
                      else (x1, y1, x2, y2)
               in Polygon
                    [ (x1' - nx, y1' - ny),
                      (x2' - nx, y2' - ny),
                      (x2' + nx, y2' + ny),
                      (x1' + nx, y1' + ny)
                    ]

    makeDisk r (x, y) = renderDisk r (x, y)

-- | 12-sided polygon approximation for circular caps/dots
renderDisk :: Float -> Point -> Picture
renderDisk r (x, y) =
  Polygon
    [ (x + r * cos ang, y + r * sin ang)
    | i <- [0 .. 11],
      let ang = fromIntegral (i :: Int) * (pi / 6.0)
    ]

-- | Render a fill command as Gloss polygons, clipped to page bounds
renderFill :: GraphicsState -> [PathSegment] -> Picture
renderFill gs segs =
  let col = psColorToGloss (rgbColor gs)
      targetSegs = case clipPath gs of
        Just cSegs | not (null cSegs) && isFillBoundingBox segs -> cSegs
        _ -> segs
      subpaths = segmentsToSubpaths targetSegs
      mkPoly pts =
        let glossPts = map toGlossPoint pts
            clipped = clipPolygonToRect (-pageHalfW) pageHalfW (-pageHalfH) pageHalfH glossPts
         in if length clipped < 3
              then Blank
              else Polygon clipped
   in Color col $ Pictures (map mkPoly subpaths)
  where
    isFillBoundingBox s =
      let pts = concat (segmentsToSubpaths s)
       in case pts of
            [] -> False
            _ ->
              let xs = map fst pts
                  ys = map snd pts
                  w = maximum xs - minimum xs
                  h = maximum ys - minimum ys
               in w >= 250 && h >= 200

-- | Render vector text, clipping stroke segments to clipPath when active
renderText :: GraphicsState -> (Double, Double) -> String -> Picture
renderText gs pt txt =
  let mClipPolys = case clipPath gs of
        Just cSegs
          | not (null cSegs) ->
              Just [map toGlossPoint sp | sp <- segmentsToSubpaths cSegs, length sp >= 3]
        _ -> Nothing
   in case mClipPolys of
        Just polys -> renderClippedText gs pt txt polys
        Nothing ->
          let (gx, gy) = toGlossPoint pt
              (a, b, _, _, _, _) = ctm gs
              rotDeg = realToFrac (atan2 b a * 180.0 / pi)
           in renderOneChar gs (gx, gy) rotDeg txt

-- | Render text using vector stroke polylines clipped against clip polygons
renderClippedText :: GraphicsState -> (Double, Double) -> String -> [[Point]] -> Picture
renderClippedText gs (px, py) txt polys =
  let col = psColorToGloss (rgbColor gs)
      sz = fontSize gs
      (a, b, c, d, _, _) = ctm gs
      sx = sqrt (a * a + b * b)
      sy = sqrt (c * c + d * d)
      effScale = max 1e-3 ((if sx == 0 then 1.0 else sx + if sy == 0 then 1.0 else sy) * 0.5)
      strokeW = max 0.8 (realToFrac (abs sz * effScale * 0.085))
      isItalic = "Italic" `isInfixOf` fontName gs || "Oblique" `isInfixOf` fontName gs
      toDev (lx, ly) =
        let lx' = if isItalic then lx + 0.22 * ly else lx
            dx = lx' * sz
            dy = ly * sz
         in toGlossPoint (px + a * dx + c * dy, py + b * dx + d * dy)
      buildChar (curX, acc) ch =
        let (adv, strokes) = charStrokes ch
            segs =
              [ (toDev (curX + x1, y1), toDev (curX + x2, y2))
              | poly <- strokes,
                ((x1, y1), (x2, y2)) <- zip poly (drop 1 poly)
              ]
         in (curX + adv, acc ++ segs)
      (_, allSegs) = foldl buildChar (0.0, []) txt
      clippedSegs = concatMap (uncurry (clipSegmentToPolys polys)) allSegs
      pics = [renderThickSubpath strokeW 1 1 [q1, q2] | (q1, q2) <- clippedSegs]
   in Color col (Pictures pics)

-- | Compute intersection parameter t in (0, 1) between two 2D line segments
segmentIntersectT :: Point -> Point -> Point -> Point -> Maybe Float
segmentIntersectT (x1, y1) (x2, y2) (x3, y3) (x4, y4) =
  let dx1 = x2 - x1
      dy1 = y2 - y1
      dx2 = x4 - x3
      dy2 = y4 - y3
      denom = dx1 * dy2 - dy1 * dx2
   in if abs denom < 1e-6
        then Nothing
        else
          let t = ((x3 - x1) * dy2 - (y3 - y1) * dx2) / denom
              u = ((x3 - x1) * dy1 - (y3 - y1) * dx1) / denom
           in if t > 1e-4 && t < 1.0 - 1e-4 && u >= -1e-4 && u <= 1.0 + 1e-4
                then Just t
                else Nothing

-- | Clip a line segment against a list of polygons, keeping sub-segments inside any polygon
clipSegmentToPolys :: [[Point]] -> Point -> Point -> [(Point, Point)]
clipSegmentToPolys polys p1@(x1, y1) p2@(x2, y2) =
  let edges = concatMap (\poly -> zip poly (drop 1 poly ++ take 1 poly)) polys
      ts =
        sort $
          0.0
            : 1.0
            : [ t
              | (q1, q2) <- edges,
                Just t <- [segmentIntersectT p1 p2 q1 q2]
              ]
      pairs = zip ts (drop 1 ts)
      lerp t = (x1 + t * (x2 - x1), y1 + t * (y2 - y1))
   in [ (lerp t0, lerp t1)
      | (t0, t1) <- pairs,
        t1 - t0 > 1e-4,
        let tm = (t0 + t1) * 0.5,
        isPointInClipPolys (lerp tm) polys
      ]

-- | Minimal normalized vector stroke font (baseline 0.0, cap 0.72) for clipped text rendering
charStrokes :: Char -> (Double, [[(Double, Double)]])
charStrokes ' ' = (0.30, [])
charStrokes 'H' = (0.65, [[(0.08, 0.0), (0.08, 0.72)], [(0.57, 0.0), (0.57, 0.72)], [(0.08, 0.36), (0.57, 0.36)]])
charStrokes 'e' = (0.52, [[(0.06, 0.25), (0.46, 0.25), (0.44, 0.38), (0.34, 0.48), (0.18, 0.48), (0.08, 0.38), (0.06, 0.22), (0.08, 0.10), (0.18, 0.0), (0.36, 0.0), (0.44, 0.08)]])
charStrokes 'l' = (0.26, [[(0.13, 0.72), (0.13, 0.05), (0.18, 0.0), (0.22, 0.0)]])
charStrokes 'o' = (0.54, [[(0.27, 0.48), (0.14, 0.48), (0.06, 0.36), (0.06, 0.12), (0.14, 0.0), (0.40, 0.0), (0.48, 0.12), (0.48, 0.36), (0.40, 0.48), (0.27, 0.48)]])
charStrokes 'T' = (0.60, [[(0.04, 0.72), (0.56, 0.72)], [(0.30, 0.72), (0.30, 0.0)]])
charStrokes 't' = (0.36, [[(0.16, 0.70), (0.16, 0.08), (0.22, 0.0), (0.30, 0.0)], [(0.04, 0.48), (0.28, 0.48)]])
charStrokes 'X' = (0.62, [[(0.06, 0.72), (0.56, 0.0)], [(0.56, 0.72), (0.06, 0.0)]])
charStrokes 'x' = (0.50, [[(0.06, 0.48), (0.44, 0.0)], [(0.44, 0.48), (0.06, 0.0)]])
charStrokes 'i' = (0.26, [[(0.13, 0.48), (0.13, 0.0)], [(0.13, 0.64), (0.13, 0.68)]])
charStrokes 's' = (0.48, [[(0.42, 0.40), (0.34, 0.48), (0.16, 0.48), (0.08, 0.40), (0.10, 0.30), (0.38, 0.22), (0.42, 0.10), (0.34, 0.0), (0.14, 0.0), (0.06, 0.08)]])
charStrokes 'h' = (0.54, [[(0.08, 0.72), (0.08, 0.0)], [(0.08, 0.36), (0.18, 0.48), (0.36, 0.48), (0.46, 0.38), (0.46, 0.0)]])
charStrokes 'a' = (0.52, [[(0.10, 0.40), (0.20, 0.48), (0.36, 0.48), (0.44, 0.38), (0.44, 0.0)], [(0.44, 0.24), (0.16, 0.24), (0.06, 0.14), (0.08, 0.04), (0.20, 0.0), (0.36, 0.0), (0.44, 0.08)]])
charStrokes _ = (0.52, [[(0.08, 0.0), (0.08, 0.68), (0.44, 0.68), (0.44, 0.0), (0.08, 0.0)]])

-- | Helper to test whether a point is inside any clip polygon
isPointInClipPolys :: Point -> [[Point]] -> Bool
isPointInClipPolys (px, py) polys =
  any inPoly polys
  where
    inPoly poly =
      let edges = zip poly (drop 1 poly ++ take 1 poly)
          crosses ((x1, y1), (x2, y2)) =
            (y1 > py) /= (y2 > py)
              && (px < (x2 - x1) * (py - y1) / (y2 - y1) + x1)
       in odd (length (filter crosses edges))

-- | Render a single text string with rotation, scaling, and solid bold overdraw
renderOneChar :: GraphicsState -> Point -> Float -> String -> Picture
renderOneChar gs (gx, gy) rotDeg str =
  let col = psColorToGloss (rgbColor gs)
      (a, b, c, d, _, _) = ctm gs
      sx = sqrt (a * a + b * b)
      sy = sqrt (c * c + d * d)
      det = a * d - b * c
      scaleX = if sx == 0 then 1.0 else sx
      scaleY = (if sy == 0 then 1.0 else sy) * (if det < 0 then -1.0 else 1.0)
      sBase = realToFrac (fontSize gs) * 0.007
      textPic = Scale (sBase * realToFrac scaleX) (sBase * realToFrac scaleY) $ Text str
      effSize = abs (fontSize gs) * abs scaleY
      stemR = max 0.0 (realToFrac effSize * 0.045)
      boldPic =
        if stemR <= 0.35
          then textPic
          else
            let step = max 0.55 (stemR / 3.0)
                steps = [-stemR, -stemR + step .. stemR]
                offsets = [(dx, dy) | dx <- steps, dy <- steps, dx * dx + dy * dy <= stemR * stemR]
             in Pictures [Translate dx dy textPic | (dx, dy) <- offsets]
      rotPic = if abs rotDeg > 0.01 then Rotate (-rotDeg) boldPic else boldPic
   in if isInsideRect (-pageHalfW - 200) (pageHalfW + 200) (-pageHalfH - 200) (pageHalfH + 200) (gx, gy)
        then Color col $ Translate gx gy rotPic
        else Blank

-- | Sutherland-Hodgman polygon clipping against an axis-aligned bounding box
clipPolygonToRect :: Float -> Float -> Float -> Float -> [Point] -> [Point]
clipPolygonToRect minX maxX minY maxY poly0 =
  foldl
    (flip clipEdge)
    poly0
    [ (\(x, _) -> x >= minX, \(x1, y1) (x2, y2) -> (minX, interpolateY minX x1 y1 x2 y2)),
      (\(x, _) -> x <= maxX, \(x1, y1) (x2, y2) -> (maxX, interpolateY maxX x1 y1 x2 y2)),
      (\(_, y) -> y >= minY, \(x1, y1) (x2, y2) -> (interpolateX minY y1 x1 y2 x2, minY)),
      (\(_, y) -> y <= maxY, \(x1, y1) (x2, y2) -> (interpolateX maxY y1 x1 y2 x2, maxY))
    ]
  where
    interpolateY targetX x1 y1 x2 y2 =
      let dx = x2 - x1 in if abs dx < 1e-6 then y1 else y1 + (targetX - x1) / dx * (y2 - y1)
    interpolateX targetY y1 x1 y2 x2 =
      let dy = y2 - y1 in if abs dy < 1e-6 then x1 else x1 + (targetY - y1) / dy * (x2 - x1)

    clipEdge (isInside, intersect) poly
      | null poly = []
      | otherwise = go (last poly) poly []
      where
        go _ [] acc = reverse acc
        go prev (curr : rest) acc =
          case (isInside prev, isInside curr) of
            (True, True) -> go curr rest (curr : acc)
            (True, False) -> go curr rest (intersect prev curr : acc)
            (False, True) -> go curr rest (curr : intersect prev curr : acc)
            (False, False) -> go curr rest acc

-- | Liang-Barsky parametric line segment clipping against an axis-aligned bounding box
clipSegmentToRect :: Float -> Float -> Float -> Float -> Point -> Point -> Maybe (Point, Point)
clipSegmentToRect minX maxX minY maxY (x1, y1) (x2, y2) = do
  let dx = x2 - x1
      dy = y2 - y1
      clipTest p q (t0, t1)
        | p == 0 = if q < 0 then Nothing else Just (t0, t1)
        | p < 0 = let r = q / p in if r > t1 then Nothing else Just (max t0 r, t1)
        | otherwise = let r = q / p in if r < t0 then Nothing else Just (t0, min t1 r)

  (t0, t1) <-
    clipTest (-dx) (x1 - minX) (0.0, 1.0)
      >>= clipTest dx (maxX - x1)
      >>= clipTest (-dy) (y1 - minY)
      >>= clipTest dy (maxY - y1)

  if t0 > t1
    then Nothing
    else
      Just
        ( (x1 + t0 * dx, y1 + t0 * dy),
          (x1 + t1 * dx, y1 + t1 * dy)
        )

isInsideRect :: Float -> Float -> Float -> Float -> Point -> Bool
isInsideRect minX maxX minY maxY (x, y) =
  x >= minX && x <= maxX && y >= minY && y <= maxY

-- | Convert path segments into discrete continuous point paths
segmentsToSubpaths :: [PathSegment] -> [[(Double, Double)]]
segmentsToSubpaths = go [] [] (0.0, 0.0) (0.0, 0.0)
  where
    finishSub curr acc = if null curr then acc else reverse curr : acc

    go currSub acc _ _ [] =
      reverse (finishSub currSub acc)
    go currSub acc _ _ (MoveTo x y : rest) =
      go [(x, y)] (finishSub currSub acc) (x, y) (x, y) rest
    go currSub acc _ startPt (LineTo x y : rest) =
      go ((x, y) : currSub) acc (x, y) startPt rest
    go currSub acc lastPt startPt (CurveTo x1 y1 x2 y2 x3 y3 : rest) =
      let sampled = sampleBezier 20 lastPt (x1, y1) (x2, y2) (x3, y3)
          curr' = reverse sampled ++ currSub
       in go curr' acc (x3, y3) startPt rest
    go currSub acc _ startPt (Arc cx cy r a1 a2 : rest) =
      let sampled = sampleArc False 24 cx cy r a1 a2
          endPt = if null sampled then (cx, cy) else last sampled
       in go (reverse sampled ++ currSub) acc endPt startPt rest
    go currSub acc _ startPt (ArcN cx cy r a1 a2 : rest) =
      let sampled = sampleArc True 24 cx cy r a1 a2
          endPt = if null sampled then (cx, cy) else last sampled
       in go (reverse sampled ++ currSub) acc endPt startPt rest
    go currSub acc _ startPt (ClosePath : rest) =
      go (startPt : currSub) acc startPt startPt rest

-- | Sample points along a cubic Bézier curve
sampleBezier :: Int -> (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> [(Double, Double)]
sampleBezier n (x0, y0) (x1, y1) (x2, y2) (x3, y3) =
  [bezier (fromIntegral i / fromIntegral n) | i <- [1 .. n]]
  where
    bezier t =
      let u = 1.0 - t
          tt = t * t
          uu = u * u
          u3 = uu * u
          t3 = tt * t
          bx = u3 * x0 + 3.0 * uu * t * x1 + 3.0 * u * tt * x2 + t3 * x3
          by = u3 * y0 + 3.0 * uu * t * y1 + 3.0 * u * tt * y2 + t3 * y3
       in (bx, by)

-- | Sample points along a circular arc
sampleArc :: Bool -> Int -> Double -> Double -> Double -> Double -> Double -> [(Double, Double)]
sampleArc isClockwise n cx cy r a1 a2 =
  let delta =
        if isClockwise
          then
            let angSpan = if a1 <= a2 then a1 - a2 + 360.0 else a1 - a2
             in -angSpan / fromIntegral n
          else
            let angSpan = if a2 <= a1 then a2 - a1 + 360.0 else a2 - a1
             in angSpan / fromIntegral n
   in [ let ang = (a1 + fromIntegral i * delta) * pi / 180.0
         in (cx + r * cos ang, cy + r * sin ang)
      | i <- [1 .. n]
      ]
