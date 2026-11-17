module PostScript.Primitives.Graphics.Text
  ( psFindfont,
    psScalefont,
    psSetfont,
    psCurrentfont,
    psShow,
    psAshow,
    psWidthshow,
    psAwidthshow,
    psKshow,
    psStringwidth,
    psCharpath,
    psMakefont,
    psDefinefont,
  )
where

import Control.Monad (when)
import Control.Monad.State.Strict (gets, modify)
import Data.List (foldl')
import Data.Map.Strict qualified as Map
import PostScript.Environment (defineSymbol, lookupDict, modifyGState, newDict, pop, push)
import PostScript.Primitives.Graphics.Render (renderTextToSVG)
import PostScript.Primitives.Graphics.Transform (invertMatrix, transformPoint)
import PostScript.Types

-- Pop string-like object as Haskell String
popStr :: Interpreter String
popStr = do
  obj <- pop
  case obj of
    PSString s -> return s
    PSName s -> return s
    PSSymbol s -> return s
    PSInteger n -> return (show n)
    PSReal r -> return (show r)
    _ -> error "typecheck: expected string or name"

-- Pop numeric value as Double
popNum :: Interpreter Double
popNum = do
  obj <- pop
  case obj of
    PSInteger n -> return (fromIntegral n)
    PSReal r -> return r
    _ -> error "typecheck: expected number"

-- 'findfont': name findfont -> font
psFindfont :: Interpreter ()
psFindfont = do
  name <- popStr
  mFound <- lookupDict name
  case mFound of
    Just fontObj@(PSDict _) -> push fontObj
    _ -> do
      fDict <- newDict (Map.fromList [("FontName", PSName name), ("Scale", PSReal 1.0)])
      push fDict

-- 'scalefont': font scale scalefont -> scaled font
psScalefont :: Interpreter ()
psScalefont = do
  scale <- popNum
  fontObj <- pop
  case fontObj of
    PSDict did -> do
      heap <- gets dictHeap
      let d = Map.findWithDefault Map.empty did heap
          fName = Map.findWithDefault (PSName "Helvetica") "FontName" d
          oldScale = case Map.lookup "Scale" d of
            Just (PSReal s) -> s
            Just (PSInteger n) -> fromIntegral n
            _ -> 1.0
      newDict (Map.insert "Scale" (PSReal (scale * oldScale)) (Map.insert "FontName" fName d)) >>= push
    _ -> error "typecheck: scalefont expected font dictionary and number"

-- 'setfont': font setfont -> set active font in GraphicsState
psSetfont :: Interpreter ()
psSetfont = do
  fontObj <- pop
  case fontObj of
    PSDict did -> do
      heap <- gets dictHeap
      let d = Map.findWithDefault Map.empty did heap
          fName = case Map.lookup "FontName" d of
            Just (PSName s) -> s
            Just (PSString s) -> s
            Just (PSSymbol s) -> s
            _ -> "Helvetica"
          fSize = case Map.lookup "Scale" d of
            Just (PSReal s) -> s
            Just (PSInteger n) -> fromIntegral n
            _ -> 10.0
      modifyGState (\gs -> gs {fontName = fName, fontSize = fSize})
    _ -> error "typecheck: setfont expected font dictionary"

-- 'currentfont': currentfont -> font
psCurrentfont :: Interpreter ()
psCurrentfont = do
  gs <- gets gState
  newDict (Map.fromList [("FontName", PSName (fontName gs)), ("Scale", PSReal (fontSize gs))]) >>= push

-- 'show': string show -> render string at currentPoint transformed by CTM
psShow :: Interpreter ()
psShow = do
  txt <- popStr
  gs <- gets gState
  case currentPoint gs of
    Nothing -> error "nocurrentpoint: show requires an active current point"
    Just ptD -> do
      let svgElem = renderTextToSVG gs ptD txt
          cmd = DrawText gs ptD txt
      modify $ \s ->
        s
          { canvasElements = svgElem : canvasElements s,
            graphicCommands = cmd : graphicCommands s
          }
      -- Advance current point in user coordinates, then project back to device coordinates
      let m = ctm gs
          mInv = invertMatrix m
          (xu, yu) = case mInv of
            Just inv -> transformPoint inv ptD
            Nothing -> ptD
          advanceWidth = fromIntegral (length txt) * fontSize gs * 0.55
          newPt = transformPoint m (xu + advanceWidth, yu)
      modifyGState (\s' -> s' {currentPoint = Just newPt})

-- 'stringwidth': string stringwidth -> wx wy
psStringwidth :: Interpreter ()
psStringwidth = do
  txt <- popStr
  sz <- gets (fontSize . gState)
  let w = fromIntegral (length txt) * sz * 0.55
  push (PSReal w) >> push (PSReal 0.0)

-- Helper for showing text with extra spacing
showWithExtraSpacing :: (Char -> (Double, Double)) -> String -> Interpreter ()
showWithExtraSpacing getExtra txt = do
  gs <- gets gState
  case currentPoint gs of
    Nothing -> error "nocurrentpoint: show requires an active current point"
    Just _ -> mapM_ showOneChar txt
  where
    showOneChar c = do
      gs <- gets gState
      case currentPoint gs of
        Nothing -> return ()
        Just ptD -> do
          let svgElem = renderTextToSVG gs ptD [c]
              cmd = DrawText gs ptD [c]
          modify $ \s ->
            s
              { canvasElements = svgElem : canvasElements s,
                graphicCommands = cmd : graphicCommands s
              }
          let m = ctm gs
              mInv = invertMatrix m
              (xu, yu) = case mInv of
                Just inv -> transformPoint inv ptD
                Nothing -> ptD
              charWidth = fontSize gs * 0.55
              (extraX, extraY) = getExtra c
              newPt = transformPoint m (xu + charWidth + extraX, yu + extraY)
          modifyGState (\s' -> s' {currentPoint = Just newPt})

-- 'ashow': ax ay string ashow -> -
psAshow :: Interpreter ()
psAshow = do
  txt <- popStr
  ay <- popNum
  ax <- popNum
  showWithExtraSpacing (\_ -> (ax, ay)) txt

-- 'widthshow': cx cy char string widthshow -> -
psWidthshow :: Interpreter ()
psWidthshow = do
  txt <- popStr
  cObj <- pop
  cy <- popNum
  cx <- popNum
  let targetCode = case cObj of
        PSInteger n -> fromIntegral n
        _ -> 32
  showWithExtraSpacing (\c -> if fromEnum c == targetCode then (cx, cy) else (0.0, 0.0)) txt

-- 'awidthshow': cx cy char ax ay string awidthshow -> -
psAwidthshow :: Interpreter ()
psAwidthshow = do
  txt <- popStr
  ay <- popNum
  ax <- popNum
  cObj <- pop
  cy <- popNum
  cx <- popNum
  let targetCode = case cObj of
        PSInteger n -> fromIntegral n
        _ -> 32
  showWithExtraSpacing
    ( \c ->
        let (cx', cy') = if fromEnum c == targetCode then (cx, cy) else (0.0, 0.0)
         in (ax + cx', ay + cy')
    )
    txt

-- 'kshow': proc string kshow -> -
psKshow :: ([Object] -> Interpreter ()) -> Interpreter ()
psKshow evalProg = do
  txt <- popStr
  procObj <- pop
  let len = length txt
      execProc = case procObj of
        PSBlock objs -> evalProg objs
        PSSymbol _ -> evalProg [procObj]
        PSOperator _ -> evalProg [procObj]
        _ -> push procObj
  mapM_
    ( \(i, c) -> do
        gs <- gets gState
        case currentPoint gs of
          Nothing -> error "nocurrentpoint: kshow requires an active current point"
          Just ptD -> do
            let svgElem = renderTextToSVG gs ptD [c]
                cmd = DrawText gs ptD [c]
            modify $ \s ->
              s
                { canvasElements = svgElem : canvasElements s,
                  graphicCommands = cmd : graphicCommands s
                }
            let m = ctm gs
                mInv = invertMatrix m
                (xu, yu) = case mInv of
                  Just inv -> transformPoint inv ptD
                  Nothing -> ptD
                charWidth = fontSize gs * 0.55
                newPt = transformPoint m (xu + charWidth, yu)
            modifyGState (\s' -> s' {currentPoint = Just newPt})
            when (i < len - 1) $ do
              let cPrev = fromEnum c :: Int
                  cNext = fromEnum (txt !! (i + 1)) :: Int
              push (PSInteger cPrev)
              push (PSInteger cNext)
              execProc
    )
    (zip [0 ..] txt)

-- | Glyph outline polygonal subpaths (in [0, 1] unit square coordinates)
glyphOutlines :: Char -> (Double, [[(Double, Double)]])
glyphOutlines 't' =
  ( 0.38,
    [ [ (0.10, 0.75),
        (0.22, 0.75),
        (0.22, 0.58),
        (0.35, 0.58),
        (0.35, 0.48),
        (0.22, 0.48),
        (0.22, 0.10),
        (0.28, 0.08),
        (0.35, 0.12),
        (0.35, 0.00),
        (0.22, 0.00),
        (0.10, 0.05),
        (0.10, 0.48),
        (0.02, 0.48),
        (0.02, 0.58),
        (0.10, 0.58)
      ]
    ]
  )
glyphOutlines 'e' =
  ( 0.54,
    [ [ (0.05, 0.26),
        (0.08, 0.10),
        (0.20, 0.00),
        (0.36, 0.00),
        (0.48, 0.08),
        (0.44, 0.16),
        (0.34, 0.09),
        (0.22, 0.09),
        (0.16, 0.16),
        (0.16, 0.25),
        (0.50, 0.25),
        (0.50, 0.34),
        (0.44, 0.46),
        (0.30, 0.53),
        (0.16, 0.46),
        (0.08, 0.36)
      ],
      [(0.16, 0.33), (0.40, 0.33), (0.36, 0.44), (0.28, 0.46), (0.18, 0.42)]
    ]
  )
glyphOutlines 'x' =
  ( 0.50,
    [ [ (0.02, 0.52),
        (0.15, 0.52),
        (0.25, 0.35),
        (0.35, 0.52),
        (0.48, 0.52),
        (0.33, 0.26),
        (0.48, 0.00),
        (0.35, 0.00),
        (0.25, 0.17),
        (0.15, 0.00),
        (0.02, 0.00),
        (0.17, 0.26)
      ]
    ]
  )
glyphOutlines 'X' =
  ( 0.64,
    [ [ (0.02, 0.75),
        (0.18, 0.75),
        (0.32, 0.50),
        (0.46, 0.75),
        (0.62, 0.75),
        (0.42, 0.38),
        (0.62, 0.00),
        (0.46, 0.00),
        (0.32, 0.25),
        (0.18, 0.00),
        (0.02, 0.00),
        (0.22, 0.38)
      ]
    ]
  )
glyphOutlines 'T' =
  ( 0.60,
    [ [ (0.04, 0.75),
        (0.56, 0.75),
        (0.56, 0.63),
        (0.36, 0.63),
        (0.36, 0.00),
        (0.24, 0.00),
        (0.24, 0.63),
        (0.04, 0.63)
      ]
    ]
  )
glyphOutlines ' ' = (0.30, [])
glyphOutlines _ =
  ( 0.55,
    [ [(0.06, 0.70), (0.49, 0.70), (0.49, 0.00), (0.06, 0.00)],
      [(0.16, 0.60), (0.39, 0.60), (0.39, 0.10), (0.16, 0.10)]
    ]
  )

-- 'charpath': string bool charpath -> append glyph outlines to currentPath
psCharpath :: Interpreter ()
psCharpath = do
  _isStroke <- pop
  txt <- popStr
  gs <- gets gState
  case currentPoint gs of
    Nothing -> error "nocurrentpoint: charpath requires an active current point"
    Just ptD -> do
      let m = ctm gs
          mInv = invertMatrix m
          (xu, yu) = case mInv of
            Just inv -> transformPoint inv ptD
            Nothing -> ptD
          sz = fontSize gs
          (finalXu, allSegsRev) = foldl' appendChar (xu, []) txt
          appendChar (curX, accSegs) c =
            let (adv, subpaths) = glyphOutlines c
                makeSubpath [] = []
                makeSubpath ((x0, y0) : restPts) =
                  let p0D = transformPoint m (curX + x0 * sz, yu + y0 * sz)
                      restD = [transformPoint m (curX + x * sz, yu + y * sz) | (x, y) <- restPts]
                      move = MoveTo (fst p0D) (snd p0D)
                      lines' = [LineTo (fst p) (snd p) | p <- restD]
                   in move : lines' ++ [ClosePath]
                charSegs = concatMap makeSubpath subpaths
             in (curX + adv * sz, reverse charSegs ++ accSegs)
          newPt = transformPoint m (finalXu, yu)
      modifyGState
        ( \s ->
            s
              { currentPath = allSegsRev ++ currentPath s,
                currentPoint = Just newPt
              }
        )

-- 'makefont': font matrix makefont -> apply matrix to font
psMakefont :: Interpreter ()
psMakefont = do
  mObj <- pop
  fontObj <- pop
  case (fontObj, mObj) of
    (PSDict did, PSArray aid) -> do
      arrHeap <- gets arrayHeap
      heap <- gets dictHeap
      case Map.lookup aid arrHeap of
        Just [PSReal a, PSReal b, PSReal c, PSReal d, _, _] -> do
          let detScale = sqrt (abs (a * d - b * c))
              oldDict = Map.findWithDefault Map.empty did heap
              fName = Map.findWithDefault (PSName "Helvetica") "FontName" oldDict
              oldScale = case Map.lookup "Scale" oldDict of
                Just (PSReal s) -> s
                Just (PSInteger n) -> fromIntegral n
                _ -> 1.0
          newDict (Map.insert "Scale" (PSReal (detScale * oldScale)) (Map.insert "FontName" fName oldDict)) >>= push
        _ -> push fontObj
    _ -> error "typecheck: makefont expected font and matrix"

-- 'definefont': name font definefont -> register custom font dictionary
psDefinefont :: Interpreter ()
psDefinefont = do
  fontObj <- pop
  name <- popStr
  defineSymbol name fontObj
  push fontObj
