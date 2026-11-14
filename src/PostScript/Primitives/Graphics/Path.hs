module PostScript.Primitives.Graphics.Path
  ( psNewpath,
    psMoveto,
    psRmoveto,
    psLineto,
    psRlineto,
    psCurveto,
    psRcurveto,
    psArc,
    psArcn,
    psArcto,
    psClosepath,
    psCurrentpoint,
    psPathbbox,
    psFlattenpath,
    psPathforall,
    psClippath,
    psStrokepath,
  )
where

import Control.Monad.State.Strict (gets)
import PostScript.Environment
  ( loopWhile,
    modifyGState,
    pop,
    push,
    toCode,
  )
import PostScript.Primitives.Graphics.Transform
  ( invertMatrix,
    transformPoint,
  )
import PostScript.Types
  ( GraphicsState (..),
    Interpreter,
    Matrix,
    Object (..),
    PSState (..),
    PathSegment (..),
  )

-- Helpers for coordinate conversion and stack manipulation

popNum :: Interpreter Double
popNum =
  pop >>= \case
    PSInteger n -> return (fromIntegral n)
    PSReal r -> return r
    _ -> error "typecheck: expected number"

popPoint :: Interpreter (Double, Double)
popPoint = do
  y <- popNum
  x <- popNum
  return (x, y)

pushPt :: (Double, Double) -> Interpreter ()
pushPt (x, y) = push (PSReal x) >> push (PSReal y)

-- Helper to retrieve current point in user coordinates, or raise error
getUserCurrentPoint :: String -> Interpreter (Double, Double, Matrix)
getUserCurrentPoint op = do
  gs <- gets gState
  case currentPoint gs of
    Nothing -> error ("nocurrentpoint: no current point in " ++ op)
    Just ptD -> case invertMatrix (ctm gs) of
      Nothing -> error ("undefinedresult: singular matrix in " ++ op)
      Just mInv ->
        let (xu, yu) = transformPoint mInv ptD
         in return (xu, yu, ctm gs)

-- Helper to append a path segment and update currentPoint
addEndpoint :: (Double -> Double -> PathSegment) -> (Double, Double) -> Interpreter ()
addEndpoint mkSeg ptD =
  modifyGState
    ( \s ->
        s
          { currentPath = mkSeg (fst ptD) (snd ptD) : currentPath s,
            currentPoint = Just ptD
          }
    )

-- Path construction primitives

-- 'newpath': clears currentPath and resets currentPoint
psNewpath :: Interpreter ()
psNewpath = modifyGState (\gs -> gs {currentPath = [], currentPoint = Nothing})

-- 'currentpoint': pushes current position in user coordinates
psCurrentpoint :: Interpreter ()
psCurrentpoint = do
  (xu, yu, _) <- getUserCurrentPoint "currentpoint"
  pushPt (xu, yu)

-- 'moveto': x y moveto -> starts a new subpath at (x, y)
psMoveto :: Interpreter ()
psMoveto = do
  pt <- popPoint
  m <- gets (ctm . gState)
  addEndpoint MoveTo (transformPoint m pt)

-- 'rmoveto': dx dy rmoveto -> relative move in user coordinates
psRmoveto :: Interpreter ()
psRmoveto = do
  (dx, dy) <- popPoint
  (xu, yu, m) <- getUserCurrentPoint "rmoveto"
  addEndpoint MoveTo (transformPoint m (xu + dx, yu + dy))

-- 'lineto': x y lineto -> append straight line segment to (x, y)
psLineto :: Interpreter ()
psLineto = do
  pt <- popPoint
  (_, _, m) <- getUserCurrentPoint "lineto"
  addEndpoint LineTo (transformPoint m pt)

-- 'rlineto': dx dy rlineto -> relative line in user coordinates
psRlineto :: Interpreter ()
psRlineto = do
  (dx, dy) <- popPoint
  (xu, yu, m) <- getUserCurrentPoint "rlineto"
  addEndpoint LineTo (transformPoint m (xu + dx, yu + dy))

-- 'curveto': x1 y1 x2 y2 x3 y3 curveto -> append cubic bezier curve
psCurveto :: Interpreter ()
psCurveto = do
  p3 <- popPoint
  p2 <- popPoint
  p1 <- popPoint
  (_, _, m) <- getUserCurrentPoint "curveto"
  let (p1d, p2d, p3d) = (transformPoint m p1, transformPoint m p2, transformPoint m p3)
  addEndpoint (\_ _ -> CurveTo (fst p1d) (snd p1d) (fst p2d) (snd p2d) (fst p3d) (snd p3d)) p3d

-- 'rcurveto': relative cubic bezier curve
psRcurveto :: Interpreter ()
psRcurveto = do
  (dx3, dy3) <- popPoint
  (dx2, dy2) <- popPoint
  (dx1, dy1) <- popPoint
  (xu, yu, m) <- getUserCurrentPoint "rcurveto"
  let (p1d, p2d, p3d) =
        ( transformPoint m (xu + dx1, yu + dy1),
          transformPoint m (xu + dx2, yu + dy2),
          transformPoint m (xu + dx3, yu + dy3)
        )
  addEndpoint (\_ _ -> CurveTo (fst p1d) (snd p1d) (fst p2d) (snd p2d) (fst p3d) (snd p3d)) p3d

-- 'closepath': close current subpath back to most recent moveto
psClosepath :: Interpreter ()
psClosepath = do
  gs <- gets gState
  case [pt | MoveTo x y <- currentPath gs, let pt = (x, y)] of
    (startPt : _) ->
      modifyGState
        ( \s ->
            s
              { currentPath = ClosePath : currentPath s,
                currentPoint = Just startPt
              }
        )
    [] -> return ()

-- 'arc': x y r a1 a2 arc -> counter-clockwise circular arc
psArc :: Interpreter ()
psArc = addArc False

-- 'arcn': x y r a1 a2 arcn -> clockwise circular arc
psArcn :: Interpreter ()
psArcn = addArc True

-- Helper: subdivide arc into angular spans of at most 90 degrees
arcAngleSpans :: Bool -> Double -> Double -> [(Double, Double)]
arcAngleSpans False a1 a2 =
  let diff = a2 - a1
      sweep
        | diff == 0.0 = 0.0
        | diff > 0.0 =
            let m = diff - fromIntegral (floor (diff / 360.0) :: Int) * 360.0
             in if m == 0.0 then 360.0 else m
        | otherwise =
            let m = diff - fromIntegral (floor (diff / 360.0) :: Int) * 360.0
             in if m == 0.0 then 360.0 else m
      n = max 1 (ceiling (sweep / 90.0) :: Int)
      step = sweep / fromIntegral n
   in [ (a1 + fromIntegral i * step, a1 + fromIntegral (i + 1) * step)
      | i <- [0 .. n - 1]
      ]
arcAngleSpans True a1 a2 =
  let diff = a1 - a2
      sweep
        | diff == 0.0 = 0.0
        | diff > 0.0 =
            let m = diff - fromIntegral (floor (diff / 360.0) :: Int) * 360.0
             in if m == 0.0 then 360.0 else m
        | otherwise =
            let m = diff - fromIntegral (floor (diff / 360.0) :: Int) * 360.0
             in if m == 0.0 then 360.0 else m
      n = max 1 (ceiling (sweep / 90.0) :: Int)
      step = sweep / fromIntegral n
   in [ (a1 - fromIntegral i * step, a1 - fromIntegral (i + 1) * step)
      | i <- [0 .. n - 1]
      ]

addArc :: Bool -> Interpreter ()
addArc isClockwise = do
  a2 <- popNum
  a1 <- popNum
  r <- popNum
  (cx, cy) <- popPoint
  gs <- gets gState
  let m = ctm gs
      spans = arcAngleSpans isClockwise a1 a2
      firstAngle = case spans of (s : _) -> fst s; [] -> a1
      radStart = firstAngle * pi / 180.0
      startD = transformPoint m (cx + r * cos radStart, cy + r * sin radStart)
      leadSeg = case currentPoint gs of
        Nothing -> [MoveTo (fst startD) (snd startD)]
        Just ptD
          | abs (fst ptD - fst startD) < 1e-4 && abs (snd ptD - snd startD) < 1e-4 -> []
          | otherwise -> [LineTo (fst startD) (snd startD)]

      spanToCurve (phi1Deg, phi2Deg) =
        let rad1 = phi1Deg * pi / 180.0
            rad2 = phi2Deg * pi / 180.0
            deltaRad = rad2 - rad1
            k = (4.0 / 3.0) * tan (deltaRad / 4.0)
            p0 = (cx + r * cos rad1, cy + r * sin rad1)
            p3 = (cx + r * cos rad2, cy + r * sin rad2)
            p1 = (fst p0 - k * r * sin rad1, snd p0 + k * r * cos rad1)
            p2 = (fst p3 + k * r * sin rad2, snd p3 - k * r * cos rad2)
            p1D = transformPoint m p1
            p2D = transformPoint m p2
            p3D = transformPoint m p3
         in (CurveTo (fst p1D) (snd p1D) (fst p2D) (snd p2D) (fst p3D) (snd p3D), p3D)

      curvesAndEnds = map spanToCurve spans
      curveSegs = map fst curvesAndEnds
      finalPtD = if null curvesAndEnds then startD else snd (last curvesAndEnds)

  modifyGState
    ( \s ->
        s
          { currentPath = reverse curveSegs ++ leadSeg ++ currentPath s,
            currentPoint = Just finalPtD
          }
    )

-- 'strokepath': - -> - (replaces current path with stroked outline polygons)
psStrokepath :: Interpreter ()
psStrokepath = modifyGState $ \gs ->
  let segs = reverse (currentPath gs)
      subpaths = extractSubpaths segs
      hw = max 0.5 (lineWidth gs / 2.0)
      join = lineJoin gs
      cap = lineCap gs
      quadSegs (p1, p2) =
        let dx = fst p2 - fst p1
            dy = snd p2 - snd p1
            len = sqrt (dx * dx + dy * dy)
         in if len < 1e-6
              then []
              else
                let nx = (-dy / len) * hw
                    ny = (dx / len) * hw
                 in [ MoveTo (fst p1 - nx) (snd p1 - ny),
                      LineTo (fst p2 - nx) (snd p2 - ny),
                      LineTo (fst p2 + nx) (snd p2 + ny),
                      LineTo (fst p1 + nx) (snd p1 + ny),
                      ClosePath
                    ]
      diskPoly pt =
        let pts =
              [ ( fst pt + hw * cos (fromIntegral i * pi / 6.0),
                  snd pt + hw * sin (fromIntegral i * pi / 6.0)
                )
                | i <- [0 .. 11 :: Int]
              ]
         in case pts of
              (d0 : drest) -> MoveTo (fst d0) (snd d0) : [LineTo (fst p) (snd p) | p <- drest] ++ [ClosePath]
              [] -> []
      strokeSubpath (p0 : prest@(_ : _)) =
        let pts = p0 : prest
            segQuads = concatMap quadSegs (zip pts prest)
            jointDisks =
              if join == 1 && length pts > 2
                then concatMap diskPoly (init prest)
                else []
            endDisks =
              if cap == 1 && p0 /= last prest
                then diskPoly p0 ++ diskPoly (last prest)
                else []
         in segQuads ++ jointDisks ++ endDisks
      strokeSubpath _ = []
      strokedOutlines = concatMap strokeSubpath subpaths
   in if null strokedOutlines
        then gs
        else gs {currentPath = reverse strokedOutlines}

-- | Subdivide segments into continuous coordinate subpaths
extractSubpaths :: [PathSegment] -> [[(Double, Double)]]
extractSubpaths = go [] [] (0.0, 0.0) (0.0, 0.0)
  where
    finishSub curr acc = if null curr then acc else reverse curr : acc
    go curr acc _ _ [] = reverse (finishSub curr acc)
    go curr acc _ _ (MoveTo x y : rest) = go [(x, y)] (finishSub curr acc) (x, y) (x, y) rest
    go curr acc _ s (LineTo x y : rest) = go ((x, y) : curr) acc (x, y) s rest
    go curr acc _ s (ClosePath : rest) = go (s : curr) acc s s rest
    go curr acc _ s (CurveTo _ _ _ _ x3 y3 : rest) = go ((x3, y3) : curr) acc (x3, y3) s rest
    go curr acc _ s (Arc cx cy r _ _ : rest) = go ((cx + r, cy) : curr) acc (cx + r, cy) s rest
    go curr acc _ s (ArcN cx cy r _ _ : rest) = go ((cx + r, cy) : curr) acc (cx + r, cy) s rest

-- 'arcto': x1 y1 x2 y2 r arcto -> tangent arc, leaves tangent points on stack
psArcto :: Interpreter ()
psArcto = do
  r <- popNum
  (x2, y2) <- popPoint
  (x1, y1) <- popPoint
  (x0, y0, m) <- getUserCurrentPoint "arcto"
  let (v1, v2) = ((x0 - x1, y0 - y1), (x2 - x1, y2 - y1))
      len1 = sqrt (fst v1 * fst v1 + snd v1 * snd v1)
      len2 = sqrt (fst v2 * fst v2 + snd v2 * snd v2)
      fallback = do
        let p1D = transformPoint m (x1, y1)
        addEndpoint LineTo p1D
        pushPt (x1, y1) >> pushPt (x1, y1)
  if len1 == 0 || len2 == 0 || r == 0
    then fallback
    else do
      let (u1x, u1y) = (fst v1 / len1, snd v1 / len1)
          (u2x, u2y) = (fst v2 / len2, snd v2 / len2)
          cosAlpha = max (-1.0) (min 1.0 (u1x * u2x + u1y * u2y))
          sinAlpha = sqrt (max 0.0 (1.0 - cosAlpha * cosAlpha))
      if sinAlpha < 1e-9
        then fallback
        else do
          let dist = r * (1.0 + cosAlpha) / sinAlpha
              t1 = (x1 + dist * u1x, y1 + dist * u1y)
              t2 = (x1 + dist * u2x, y1 + dist * u2y)
              (t1D, t2D) = (transformPoint m t1, transformPoint m t2)
          modifyGState
            ( \s ->
                s
                  { currentPath = LineTo (fst t2D) (snd t2D) : LineTo (fst t1D) (snd t1D) : currentPath s,
                    currentPoint = Just t2D
                  }
            )
          pushPt t1 >> pushPt t2

-- 'pathbbox': -> llx lly urx ury (push bounding box of current path)
psPathbbox :: Interpreter ()
psPathbbox = do
  gs <- gets gState
  if null (currentPath gs)
    then error "nocurrentpoint: current path is empty in pathbbox"
    else case invertMatrix (ctm gs) of
      Nothing -> error "undefinedresult: singular matrix in pathbbox"
      Just mInv -> do
        let samplePts = map (transformPoint mInv) (concatMap segmentPoints (currentPath gs))
        if null samplePts
          then error "nocurrentpoint: no points in pathbbox"
          else do
            let (xs, ys) = (map fst samplePts, map snd samplePts)
            push (PSReal (minimum xs)) >> push (PSReal (minimum ys))
            push (PSReal (maximum xs)) >> push (PSReal (maximum ys))

-- 'flattenpath': converts curves and arcs in currentPath to approximating line segments
psFlattenpath :: Interpreter ()
psFlattenpath = modifyGState (\s -> s {currentPath = reverse (flattenSegments (reverse (currentPath s)))})

-- 'pathforall': move line curve close pathforall -> iterate path segments
psPathforall :: ([Object] -> Interpreter ()) -> Interpreter ()
psPathforall eval = do
  closeProc <- pop
  curveProc <- pop
  lineProc <- pop
  moveProc <- pop
  gs <- gets gState
  case invertMatrix (ctm gs) of
    Nothing -> error "undefinedresult: singular matrix in pathforall"
    Just mInv ->
      let toLines (Arc cx cy r a1 a2) = map (uncurry LineTo) (sampleArcPts False 16 cx cy r a1 a2)
          toLines (ArcN cx cy r a1 a2) = map (uncurry LineTo) (sampleArcPts True 16 cx cy r a1 a2)
          toLines seg = [seg]
          segs = concatMap toLines (reverse (currentPath gs))
          actions = map (stepSegment mInv eval moveProc lineProc curveProc closeProc) segs
       in loopWhile actions

stepSegment :: Matrix -> ([Object] -> Interpreter ()) -> Object -> Object -> Object -> Object -> PathSegment -> Interpreter ()
stepSegment mInv eval moveProc lineProc curveProc closeProc seg = case seg of
  MoveTo x y -> pushPt (transformPoint mInv (x, y)) >> eval (toCode moveProc)
  LineTo x y -> pushPt (transformPoint mInv (x, y)) >> eval (toCode lineProc)
  CurveTo x1 y1 x2 y2 x3 y3 -> do
    pushPt (transformPoint mInv (x1, y1))
    pushPt (transformPoint mInv (x2, y2))
    pushPt (transformPoint mInv (x3, y3))
    eval (toCode curveProc)
  ClosePath -> eval (toCode closeProc)
  _ -> return ()

segmentPoints :: PathSegment -> [(Double, Double)]
segmentPoints (MoveTo x y) = [(x, y)]
segmentPoints (LineTo x y) = [(x, y)]
segmentPoints (CurveTo x1 y1 x2 y2 x3 y3) = [(x1, y1), (x2, y2), (x3, y3)]
segmentPoints (Arc cx cy r a1 a2) = (cx, cy) : sampleArcPts False 16 cx cy r a1 a2
segmentPoints (ArcN cx cy r a1 a2) = (cx, cy) : sampleArcPts True 16 cx cy r a1 a2
segmentPoints ClosePath = []

flattenSegments :: [PathSegment] -> [PathSegment]
flattenSegments = go (0.0, 0.0)
  where
    go _ [] = []
    go _ (MoveTo x y : xs) = MoveTo x y : go (x, y) xs
    go _ (LineTo x y : xs) = LineTo x y : go (x, y) xs
    go (p0x, p0y) (CurveTo x1 y1 x2 y2 x3 y3 : xs) =
      let pts = sampleBezierPts 16 (p0x, p0y) (x1, y1) (x2, y2) (x3, y3)
       in map (uncurry LineTo) pts ++ go (x3, y3) xs
    go _ (Arc cx cy r a1 a2 : xs) =
      let pts = sampleArcPts False 16 cx cy r a1 a2
          lastPt = if null pts then (cx, cy) else last pts
       in map (uncurry LineTo) pts ++ go lastPt xs
    go _ (ArcN cx cy r a1 a2 : xs) =
      let pts = sampleArcPts True 16 cx cy r a1 a2
          lastPt = if null pts then (cx, cy) else last pts
       in map (uncurry LineTo) pts ++ go lastPt xs
    go prev (ClosePath : xs) = ClosePath : go prev xs

sampleBezierPts :: Int -> (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> [(Double, Double)]
sampleBezierPts n (x0, y0) (x1, y1) (x2, y2) (x3, y3) =
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

sampleArcPts :: Bool -> Int -> Double -> Double -> Double -> Double -> Double -> [(Double, Double)]
sampleArcPts isClockwise n cx cy r a1 a2 =
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

-- 'clippath': -> (sets current path to clipping boundary)
psClippath :: Interpreter ()
psClippath = modifyGState $ \gs ->
  case clipPath gs of
    Just segs -> gs {currentPath = reverse segs, currentPoint = Nothing}
    Nothing ->
      gs
        { currentPath =
            [ ClosePath,
              LineTo 0.0 792.0,
              LineTo 612.0 792.0,
              LineTo 612.0 0.0,
              MoveTo 0.0 0.0
            ],
          currentPoint = Just (0.0, 792.0)
        }
