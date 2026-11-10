module PostScript.Primitives.Graphics.State
  ( psGsave,
    psGrestore,
    psGrestoreall,
    psSave,
    psRestore,
    psSetrgbcolor,
    psSetgray,
    psSetcmykcolor,
    psSethsbcolor,
    psCurrentrgbcolor,
    psCurrentgray,
    psCurrentlinewidth,
    psCurrentmiterlimit,
    psSetlinewidth,
    psSetlinecap,
    psSetlinejoin,
    psSetmiterlimit,
    psSetdash,
    psStroke,
    psFill,
    psEofill,
    psClip,
    psShowpage,
  )
where

import Control.Monad (unless)
import Control.Monad.State.Strict (get, gets, modify, put)
import Data.Map.Strict qualified as Map
import PostScript.Environment (modifyGState, pop, push)
import PostScript.Primitives.Graphics.Render
  ( renderFillToSVG,
    renderStrokeToSVG,
  )
import PostScript.Types
  ( GraphicsState (..),
    Interpreter,
    Object (..),
    PSState (..),
  )

-- Pop numeric value as Double
popNum :: Interpreter Double
popNum =
  pop >>= \case
    PSInteger n -> return (fromIntegral n)
    PSReal r -> return r
    _ -> error "typecheck: expected number"

clamp01 :: Double -> Double
clamp01 x = max 0.0 (min 1.0 x)

-- Graphics state stack

-- 'gsave': push copy of current GraphicsState onto stack
psGsave :: Interpreter ()
psGsave = modify (\s -> s {gStateStack = gState s : gStateStack s})

-- 'grestore': pop and restore previous GraphicsState
psGrestore :: Interpreter ()
psGrestore = do
  st <- get
  case gStateStack st of
    (g : gs) -> put (st {gState = g, gStateStack = gs})
    [] -> return ()

-- 'grestoreall': restore bottom-most GraphicsState
psGrestoreall :: Interpreter ()
psGrestoreall = do
  st <- get
  case reverse (gStateStack st) of
    (bottom : _) -> put (st {gState = bottom, gStateStack = []})
    [] -> return ()

-- 'save': -> save (creates VM state snapshot & pushes save token)
psSave :: Interpreter ()
psSave = do
  st <- get
  let sid = nextId st
  modify
    ( \s ->
        s
          { nextId = sid + 1,
            saveSnapshots = (sid, dictHeap s, arrayHeap s, gState s) : saveSnapshots s,
            gStateStack = gState s : gStateStack s
          }
    )
  push (PSSave sid)

-- 'restore': save restore -> (restores VM state snapshot)
psRestore :: Interpreter ()
psRestore = do
  tok <- pop
  let mSid = case tok of
        PSSave sid -> Just sid
        PSInteger sid -> Just sid
        _ -> Nothing
  case mSid of
    Just sid -> do
      st <- get
      case dropWhile (\(i, _, _, _) -> i /= sid) (saveSnapshots st) of
        ((_, dHeap, aHeap, gs) : rest) ->
          put
            ( st
                { dictHeap = dHeap,
                  arrayHeap = aHeap,
                  gState = gs,
                  saveSnapshots = rest
                }
            )
        [] -> case gStateStack st of
          (g : gs) -> put (st {gState = g, gStateStack = gs})
          [] -> return ()
    Nothing -> error "restore expected save object"

-- Color operators

-- 'setrgbcolor': r g b setrgbcolor -> set current stroke/fill color
psSetrgbcolor :: Interpreter ()
psSetrgbcolor = do
  b <- popNum
  g <- popNum
  r <- popNum
  modifyGState (\gs -> gs {rgbColor = (clamp01 r, clamp01 g, clamp01 b)})

-- 'setgray': gray setgray -> set grayscale color (0=black, 1=white)
psSetgray :: Interpreter ()
psSetgray = do
  g <- popNum
  let c = clamp01 g
  modifyGState (\gs -> gs {rgbColor = (c, c, c)})

-- 'setcmykcolor': c m y k setcmykcolor -> convert CMYK to RGB and set color
psSetcmykcolor :: Interpreter ()
psSetcmykcolor = do
  k <- popNum
  y <- popNum
  m <- popNum
  c <- popNum
  let r = clamp01 ((1.0 - c) * (1.0 - k))
      g = clamp01 ((1.0 - m) * (1.0 - k))
      b = clamp01 ((1.0 - y) * (1.0 - k))
  modifyGState (\gs -> gs {rgbColor = (r, g, b)})

-- 'sethsbcolor': h s b sethsbcolor -> convert HSB to RGB and set color
psSethsbcolor :: Interpreter ()
psSethsbcolor = do
  b <- popNum
  s <- popNum
  h <- popNum
  let rgb = hsbToRgb (clamp01 h) (clamp01 s) (clamp01 b)
  modifyGState (\gs -> gs {rgbColor = rgb})

hsbToRgb :: Double -> Double -> Double -> (Double, Double, Double)
hsbToRgb h s v
  | s <= 0.0 = (v, v, v)
  | otherwise =
      let h6 = (h - fromIntegral (floor h :: Int)) * 6.0
          i = floor h6 :: Int
          f = h6 - fromIntegral i
          p = v * (1.0 - s)
          q = v * (1.0 - s * f)
          t = v * (1.0 - s * (1.0 - f))
       in case i `mod` 6 of
            0 -> (v, t, p)
            1 -> (q, v, p)
            2 -> (p, v, t)
            3 -> (p, q, v)
            4 -> (t, p, v)
            _ -> (v, p, q)

-- State queries

-- 'currentrgbcolor': -> r g b (push current color components)
psCurrentrgbcolor :: Interpreter ()
psCurrentrgbcolor = do
  (r, g, b) <- gets (rgbColor . gState)
  push (PSReal r) >> push (PSReal g) >> push (PSReal b)

-- 'currentgray': -> gray (push current gray value)
psCurrentgray :: Interpreter ()
psCurrentgray = do
  (r, g, b) <- gets (rgbColor . gState)
  let gray = 0.3 * r + 0.59 * g + 0.11 * b
  push (PSReal gray)

-- 'currentlinewidth': -> width (push current line width)
psCurrentlinewidth :: Interpreter ()
psCurrentlinewidth = do
  w <- gets (lineWidth . gState)
  push (PSReal w)

-- Stroke styling

-- 'setlinewidth': width setlinewidth -> set line width
psSetlinewidth :: Interpreter ()
psSetlinewidth = do
  w <- popNum
  modifyGState (\gs -> gs {lineWidth = max 0.0 w})

-- 'setlinecap': int setlinecap -> 0=butt, 1=round, 2=square
psSetlinecap :: Interpreter ()
psSetlinecap = do
  c <-
    pop >>= \case
      PSInteger n -> return n
      PSReal r -> return (round r)
      _ -> error "typecheck: expected integer for setlinecap"
  modifyGState (\gs -> gs {lineCap = c})

-- 'setlinejoin': int setlinejoin -> 0=miter, 1=round, 2=bevel
psSetlinejoin :: Interpreter ()
psSetlinejoin = do
  j <-
    pop >>= \case
      PSInteger n -> return n
      PSReal r -> return (round r)
      _ -> error "typecheck: expected integer for setlinejoin"
  modifyGState (\gs -> gs {lineJoin = j})

-- 'setmiterlimit': limit setmiterlimit -> set miter limit
psSetmiterlimit :: Interpreter ()
psSetmiterlimit = do
  lim <- popNum
  modifyGState (\gs -> gs {miterLimit = max 1.0 lim})

-- 'currentmiterlimit': -> limit (push current miter limit)
psCurrentmiterlimit :: Interpreter ()
psCurrentmiterlimit = do
  lim <- gets (miterLimit . gState)
  push (PSReal lim)

-- 'setdash': array offset setdash -> set dash pattern
psSetdash :: Interpreter ()
psSetdash = do
  offset <- popNum
  arrObj <- pop
  dashes <- case arrObj of
    PSArray aid -> do
      heap <- gets arrayHeap
      case Map.lookup aid heap of
        Just objs -> mapM toNum objs
        Nothing -> return []
    _ -> error "typecheck: expected array for setdash"
  modifyGState (\gs -> gs {dashArray = dashes, dashOffset = offset})
  where
    toNum (PSInteger n) = return (fromIntegral n)
    toNum (PSReal r) = return r
    toNum _ = error "typecheck: dash pattern elements must be numbers"

-- Painting & rendering actions

-- 'stroke': draw outlines of currentPath to canvas and clear path
psStroke :: Interpreter ()
psStroke = do
  gs <- gets gState
  let path = reverse (currentPath gs)
  unless (null path) $ do
    let svgElem = renderStrokeToSVG gs path
    modify (\s -> s {canvasElements = svgElem : canvasElements s})
  modifyGState (\s -> s {currentPath = [], currentPoint = Nothing})

-- 'fill': fill interior of currentPath (nonzero rule) and clear path
psFill :: Interpreter ()
psFill = do
  gs <- gets gState
  let path = reverse (currentPath gs)
  unless (null path) $ do
    let svgElem = renderFillToSVG gs False path
    modify (\s -> s {canvasElements = svgElem : canvasElements s})
  modifyGState (\s -> s {currentPath = [], currentPoint = Nothing})

-- 'eofill': fill interior of currentPath (evenodd rule) and clear path
psEofill :: Interpreter ()
psEofill = do
  gs <- gets gState
  let path = reverse (currentPath gs)
  unless (null path) $ do
    let svgElem = renderFillToSVG gs True path
    modify (\s -> s {canvasElements = svgElem : canvasElements s})
  modifyGState (\s -> s {currentPath = [], currentPoint = Nothing})

-- 'clip': intersect clipping path with currentPath (path retained)
psClip :: Interpreter ()
psClip = return ()

-- 'showpage': flush canvas to output and reset page
psShowpage :: Interpreter ()
psShowpage = modifyGState (\gs -> gs {currentPath = [], currentPoint = Nothing})
