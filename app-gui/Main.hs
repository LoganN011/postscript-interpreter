module Main (main) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, tryTakeMVar)
import GlossRender
  ( compilePages,
    pageDimensions,
    paperCanvas,
  )
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import ImageLoader
  ( loadReferencePicture,
    missingImagePlaceholder,
  )
import PostScript (graphicCommands)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeFileName)
import TestBrowser
  ( discoverTestCases,
    loadTestExecution,
  )
import Types

-- | Initial window configuration
windowWidth, windowHeight :: Int
windowWidth = 1400
windowHeight = 900

windowDisplay :: Display
windowDisplay = InWindow "PostScript Dual-View GUI" (windowWidth, windowHeight) (50, 50)

backgroundColor :: Color
backgroundColor = greyN 0.15

-- | Known estimated execution times for heavy / long-running tests
testEstimatedDuration :: String -> Maybe String
testEstimatedDuration name = lookup name heavyTestEstimates
  where
    heavyTestEstimates =
      [ ("Mandelbrot_Set", "~15 - 20s"),
        ("mandelbrot", "~15 - 20s"),
        ("mandelbrot_color", "~20 - 25s"),
        ("sierp2", "~5 - 8s"),
        ("sierp1", "~2 - 3s"),
        ("sierp3a", "~2 - 3s"),
        ("sierp3b", "~2 - 3s"),
        ("sierpent1", "~2 - 3s"),
        ("sierpinski_trial", "~3 - 4s"),
        ("sierpinski2", "~3 - 4s"),
        ("sierpinski", "~2 - 3s"),
        ("sierpinski-1", "~2 - 3s"),
        ("TreePhylum", "~3 - 5s"),
        ("tree2", "~2 - 3s"),
        ("cross", "~2 - 3s"),
        ("ifs_heart2", "~2 - 3s"),
        ("koch_snowflake1", "~2 - 3s"),
        ("FractalArrows", "~1 - 2s")
      ]

main :: IO ()
main = do
  args <- getArgs
  let targetArg = case args of
        (p : _) -> Just p
        [] -> Nothing

  putStrLn "Scanning for PostScript test files..."
  cases <- discoverTestCases targetArg
  if null cases
    then do
      putStrLn "No PostScript (.ps) files found in search paths."
      exitFailure
    else do
      putStrLn $ "Discovered " ++ show (length cases) ++ " test cases."
      initialState <- initViewerState cases
      playIO
        windowDisplay
        backgroundColor
        30
        initialState
        renderWorld
        handleEvent
        stepWorld

-- | Initialize the viewer state with the first test case and trigger background evaluation
initViewerState :: [TestCase] -> IO ViewerState
initViewerState (firstCase : rest) = do
  mvar <- newEmptyMVar
  let tcName = testName firstCase
      mEst = testEstimatedDuration tcName
      st0 =
        ViewerState
          { testCases = firstCase : rest,
            activeIndex = 0,
            viewMode = ViewSideBySide,
            currentTestPages = [paperCanvas (loadingPlaceholder tcName mEst)],
            pageIndex = 0,
            referencePages = [],
            panOffset = (0.0, 0.0),
            zoomScale = 0.78,
            isDragging = False,
            dragAnchor = (0.0, 0.0),
            showMenuList = False,
            menuScroll = 0,
            statusMessage = "Initializing...",
            currentJobId = 0,
            activeWorkerTid = Nothing,
            asyncMVar = mvar
          }
  startBackgroundJob st0 0
initViewerState [] = error "initViewerState: empty test list"

-- | Asynchronously launches direct PostScript evaluation in a background worker thread
startBackgroundJob :: ViewerState -> Int -> IO ViewerState
startBackgroundJob st rawIdx = do
  -- 0. Terminate previous background worker if still computing
  mapM_ killThread (activeWorkerTid st)

  -- 1. Drain any stale message from MVar
  _ <- tryTakeMVar (asyncMVar st)

  let total = length (testCases st)
      newIdx = (rawIdx `mod` total + total) `mod` total
      tc = testCases st !! newIdx
      name = testName tc
      newJobId = currentJobId st + 1
      (pW, pH) = pageDimensions
      mEst = testEstimatedDuration name
      loadingMsg = case mEst of
        Just est -> "Interpreting " ++ name ++ "... [Warning: Long running, " ++ est ++ "]"
        Nothing -> "Interpreting " ++ name ++ "..."

  -- 2. Load the reference images for all pages on the UI thread (< 5ms)
  refPics <- do
    pics <- mapM (\p -> loadReferencePicture p (pW - 20, pH - 20)) (refImagePaths tc)
    let processed = [paperCanvas (maybe (missingImagePlaceholder name) id mPic) | mPic <- pics]
    pure (if null processed then [paperCanvas (missingImagePlaceholder name)] else processed)

  -- 3. Spawn background worker for direct interpretation
  let mvar = asyncMVar st
      psPath = psFilePath tc
  workerTid <- forkIO $ do
    res <- loadTestExecution psPath
    case res of
      Left err ->
        putMVar mvar (newJobId, Left err, "Error: " ++ take 60 err)
      Right psSt -> do
        let cmds = reverse (graphicCommands psSt)
            pages = compilePages cmds
            msg = "OK (" ++ show (length cmds) ++ " drawing commands)"
        putMVar mvar (newJobId, Right pages, msg)

  pure
    st
      { activeIndex = newIdx,
        currentTestPages = [paperCanvas (loadingPlaceholder name mEst)],
        pageIndex = 0,
        referencePages = refPics,
        statusMessage = loadingMsg,
        currentJobId = newJobId,
        activeWorkerTid = Just workerTid
      }

-- | Frame tick: checks for completed background evaluation results
stepWorld :: Float -> ViewerState -> IO ViewerState
stepWorld _ st = do
  mResult <- tryTakeMVar (asyncMVar st)
  case mResult of
    Just (jobId, result, status)
      | jobId == currentJobId st -> do
          case result of
            Left err ->
              pure
                st
                  { currentTestPages = [paperCanvas (errorPlaceholder (testName (currentCase st)) err)],
                    statusMessage = status,
                    activeWorkerTid = Nothing
                  }
            Right pages ->
              pure
                st
                  { currentTestPages = pages,
                    pageIndex = 0,
                    statusMessage = status,
                    activeWorkerTid = Nothing
                  }
    _ -> pure st

-- | Top-level render loop: builds panels, overlays, and menu
renderWorld :: ViewerState -> IO Picture
renderWorld st = do
  let (px, py) = panOffset st
      zoom = zoomScale st

      content = case viewMode st of
        ViewSideBySide ->
          let (pW, _) = pageDimensions
              halfGap = 35.0
              leftX = -(pW / 2.0 + halfGap)
              rightX = (pW / 2.0 + halfGap)
              titleY = 415.0
           in Pictures
                [ -- Left Panel: Reference Image
                  Translate leftX 0 (currentRefPicture st),
                  Translate (leftX - 180) titleY $
                    Color (greyN 0.85) $
                      Scale 0.14 0.14 $
                        Text ("REFERENCE: " ++ currentImageLabel st),
                  -- Right Panel: Interpreted PostScript
                  Translate rightX 0 (currentInterpPicture st),
                  Translate (rightX - 180) titleY $
                    Color (greyN 0.85) $
                      Scale 0.14 0.14 $
                        Text ("INTERPRETED: " ++ currentInterpLabel st)
                ]
        ViewInterpretedOnly ->
          let titleY = 415.0
           in Pictures
                [ currentInterpPicture st,
                  Translate (-180) titleY $
                    Color (greyN 0.85) $
                      Scale 0.14 0.14 $
                        Text ("INTERPRETED: " ++ currentInterpLabel st)
                ]
        ViewReferenceOnly ->
          let titleY = 415.0
           in Pictures
                [ currentRefPicture st,
                  Translate (-180) titleY $
                    Color (greyN 0.85) $
                      Scale 0.14 0.14 $
                        Text ("REFERENCE: " ++ currentImageLabel st)
                ]

      transformedContent = Translate px py $ Scale zoom zoom content
      overlays =
        [ renderHeaderOverlay st,
          renderFooterOverlay st,
          if showMenuList st then renderMenuOverlay st else Blank
        ]

  pure $ Pictures (transformedContent : overlays)

-- | Top header bar displaying test index, name, page counter, and view mode
renderHeaderOverlay :: ViewerState -> Picture
renderHeaderOverlay st =
  let halfW = fromIntegral windowWidth / 2.0
      halfH = fromIntegral windowHeight / 2.0
      barY = halfH - 22.0
      total = length (testCases st)
      curr = activeIndex st + 1
      interpPageCount = length (currentTestPages st)
      refPageCount = length (referencePages st)
      pageCount = max interpPageCount refPageCount
      pageTxt =
        if pageCount > 1
          then "  [Page " ++ show (pageIndex st + 1) ++ "/" ++ show pageCount ++ " (PgDn / PgUp / Tab)]"
          else ""
      modeStr = case viewMode st of
        ViewSideBySide -> "Side-by-Side (3)"
        ViewInterpretedOnly -> "Interpreted Only (1)"
        ViewReferenceOnly -> "Reference Only (2)"
      infoTxt = "[" ++ show curr ++ "/" ++ show total ++ "] " ++ currentTestName st ++ pageTxt ++ "  |  " ++ statusMessage st
      modeTxt = "View: " ++ modeStr
   in Pictures
        [ Translate 0 barY $
            Color (makeColor 0.08 0.08 0.08 0.90) $
              rectangleSolid (fromIntegral windowWidth) 44.0,
          Translate (-halfW + 20) (barY - 6) $ Color white $ Scale 0.13 0.13 $ Text infoTxt,
          Translate (halfW - 260) (barY - 6) $ Color (greyN 0.75) $ Scale 0.12 0.12 $ Text modeTxt
        ]

-- | Bottom footer bar with keyboard shortcuts
renderFooterOverlay :: ViewerState -> Picture
renderFooterOverlay _st =
  let halfW = fromIntegral windowWidth / 2.0
      halfH = fromIntegral windowHeight / 2.0
      barY = -halfH + 18.0
      helpTxt = "[Right/N]: Next  |  [Left/P]: Prev  |  [PgDn/Tab]: Next Page  |  [PgUp]: Prev Page  |  [1/2/3]: View  |  [M]: Menu  |  [R]: Reload"
   in Pictures
        [ Translate 0 barY $
            Color (makeColor 0.08 0.08 0.08 0.90) $
              rectangleSolid (fromIntegral windowWidth) 36.0,
          Translate (-halfW + 20) (barY - 5) $ Color (greyN 0.75) $ Scale 0.10 0.10 $ Text helpTxt
        ]

-- | On-screen scrollable menu overlay for choosing test files
renderMenuOverlay :: ViewerState -> Picture
renderMenuOverlay st =
  let total = length (testCases st)
      scroll = menuScroll st
      visibleCount = 18
      visibleCases = take visibleCount (drop scroll (zip [0 ..] (testCases st)))
      renderItem (idx, tc) =
        let isSelected = idx == activeIndex st
            rowY = 160.0 - fromIntegral (idx - scroll) * 22.0
            marker = if isSelected then "-> " else "   "
            mEst = testEstimatedDuration (testName tc)
            warnTag = case mEst of
              Just est -> " [!] (" ++ est ++ ")"
              Nothing -> ""
            itemTxt = marker ++ "[" ++ show (idx + 1) ++ "] " ++ testName tc ++ warnTag
            col =
              if isSelected
                then makeColor 0.2 0.8 1.0 1.0
                else case mEst of
                  Just _ -> makeColor 1.0 0.75 0.3 1.0
                  Nothing -> white
         in Translate (-220) rowY $ Color col $ Scale 0.11 0.11 $ Text itemTxt
   in Pictures
        [ -- Backdrop shadow panel
          Color (makeColor 0.0 0.0 0.0 0.88) $ rectangleSolid 540 480,
          Color (greyN 0.5) $ rectangleWire 540 480,
          Translate (-240) 200 $
            Color (makeColor 1.0 0.8 0.2 1.0) $
              Scale 0.14 0.14 $
                Text "Select Test (Up/Down to navigate, Enter to choose, M to close):",
          Pictures (map renderItem visibleCases),
          Translate (-240) (-210) $
            Color (greyN 0.6) $
              Scale 0.10 0.10 $
                Text ("Showing " ++ show (scroll + 1) ++ " - " ++ show (min total (scroll + visibleCount)) ++ " of " ++ show total)
        ]

-- | Interactive event dispatcher
handleEvent :: Event -> ViewerState -> IO ViewerState
handleEvent event st = case event of
  -- Mouse Drag Pan
  EventKey (MouseButton LeftButton) Down _ (mx, my) ->
    pure st {isDragging = True, dragAnchor = (mx, my)}
  EventKey (MouseButton LeftButton) Up _ _ ->
    pure st {isDragging = False}
  EventMotion (mx, my) ->
    if isDragging st
      then do
        let (ax, ay) = dragAnchor st
            (px, py) = panOffset st
        pure st {panOffset = (px + mx - ax, py + my - ay), dragAnchor = (mx, my)}
      else pure st
  -- Mouse Wheel & Keyboard Zoom
  EventKey (MouseButton WheelUp) Down _ _ -> pure $ adjustZoom (* 1.15) st
  EventKey (MouseButton WheelDown) Down _ _ -> pure $ adjustZoom (/ 1.15) st
  EventKey (Char c) Down _ _ | c `elem` "+=" -> pure $ adjustZoom (* 1.15) st
  EventKey (Char c) Down _ _ | c `elem` "-_" -> pure $ adjustZoom (/ 1.15) st
  -- Reset View
  EventKey (SpecialKey KeySpace) Down _ _ -> pure st {panOffset = (0.0, 0.0), zoomScale = 0.78}
  EventKey (Char '0') Down _ _ -> pure st {panOffset = (0.0, 0.0), zoomScale = 0.78}
  -- View Modes
  EventKey (Char '1') Down _ _ -> pure st {viewMode = ViewInterpretedOnly}
  EventKey (Char '2') Down _ _ -> pure st {viewMode = ViewReferenceOnly}
  EventKey (Char '3') Down _ _ -> pure st {viewMode = ViewSideBySide}
  -- Test Navigation: Next Test Case
  EventKey (Char c) Down _ _ | c `elem` "nN]" -> startBackgroundJob st (activeIndex st + 1)
  EventKey (SpecialKey KeyRight) Down _ _ | not (showMenuList st) -> startBackgroundJob st (activeIndex st + 1)
  -- Test Navigation: Previous Test Case
  EventKey (Char c) Down _ _ | c `elem` "pP[" -> startBackgroundJob st (activeIndex st - 1)
  EventKey (SpecialKey KeyLeft) Down _ _ | not (showMenuList st) -> startBackgroundJob st (activeIndex st - 1)
  -- Page Navigation (Updates BOTH Reference panel and Interpreted panel in sync)
  EventKey (SpecialKey KeyPageDown) Down _ _ -> advancePageOrTest st
  EventKey (SpecialKey KeyPageUp) Down _ _ -> retreatPageOrTest st
  EventKey (SpecialKey KeyTab) Down _ _ -> nextPage st
  EventKey (Char c) Down _ _ | c `elem` ">." -> nextPage st
  EventKey (Char c) Down _ _ | c `elem` "<," -> prevPage st
  -- Reload
  EventKey (Char c) Down _ _ | c `elem` "rR" -> startBackgroundJob st (activeIndex st)
  -- Toggle Test Selection Menu
  EventKey (Char c) Down _ _ | c `elem` "mM" -> pure st {showMenuList = not (showMenuList st)}
  -- Menu Navigation (or page navigation when menu closed)
  EventKey (SpecialKey KeyDown) Down _ _ ->
    if showMenuList st
      then do
        let total = length (testCases st)
            nextIdx = min (total - 1) (activeIndex st + 1)
            nextScroll = if nextIdx >= menuScroll st + 16 then nextIdx - 15 else menuScroll st
        st' <- startBackgroundJob st nextIdx
        pure st' {menuScroll = nextScroll}
      else nextPage st
  EventKey (SpecialKey KeyUp) Down _ _ ->
    if showMenuList st
      then do
        let prevIdx = max 0 (activeIndex st - 1)
            prevScroll = if prevIdx < menuScroll st then prevIdx else menuScroll st
        st' <- startBackgroundJob st prevIdx
        pure st' {menuScroll = prevScroll}
      else prevPage st
  EventKey (SpecialKey KeyEnter) Down _ _ ->
    pure st {showMenuList = False}
  -- Quit
  EventKey (Char c) Down _ _ | c `elem` "qQ" -> quitApp st
  EventKey (SpecialKey KeyEsc) Down _ _ ->
    if showMenuList st
      then pure st {showMenuList = False}
      else quitApp st
  _ -> pure st

-- | Helper to adjust zoom scale clamped between 0.2 and 5.0
adjustZoom :: (Float -> Float) -> ViewerState -> ViewerState
adjustZoom f s = s {zoomScale = min 5.0 (max 0.2 (f (zoomScale s)))}

-- | Terminate background worker and quit application cleanly
quitApp :: ViewerState -> IO a
quitApp st = do
  mapM_ killThread (activeWorkerTid st)
  exitSuccess

-- | Cycles forward to the next page, or to the next test if on the last page
advancePageOrTest :: ViewerState -> IO ViewerState
advancePageOrTest st =
  let interpPageCount = length (currentTestPages st)
      refPageCount = length (referencePages st)
      total = max interpPageCount refPageCount
   in if pageIndex st + 1 < total
        then pure st {pageIndex = pageIndex st + 1}
        else startBackgroundJob st (activeIndex st + 1)

-- | Cycles backward to the previous page, or to the previous test if on the first page
retreatPageOrTest :: ViewerState -> IO ViewerState
retreatPageOrTest st =
  if pageIndex st > 0
    then pure st {pageIndex = pageIndex st - 1}
    else startBackgroundJob st (activeIndex st - 1)

-- | Advances both reference and interpreted panels to the next page
nextPage :: ViewerState -> IO ViewerState
nextPage st =
  let interpPageCount = length (currentTestPages st)
      refPageCount = length (referencePages st)
      total = max interpPageCount refPageCount
      nextIdx = (pageIndex st + 1) `mod` max 1 total
   in pure st {pageIndex = nextIdx}

-- | Retreats both reference and interpreted panels to the previous page
prevPage :: ViewerState -> IO ViewerState
prevPage st =
  let interpPageCount = length (currentTestPages st)
      refPageCount = length (referencePages st)
      total = max interpPageCount refPageCount
      prevIdx = (pageIndex st - 1 + total) `mod` max 1 total
   in pure st {pageIndex = prevIdx}

-- | Safely retrieves the page picture at the given index or returns fallback
getPageAt :: [Picture] -> Int -> Picture -> Picture
getPageAt [] _ fallback = fallback
getPageAt pages idx _ =
  let clamped = min (length pages - 1) (max 0 idx)
   in pages !! clamped

-- | Access the active interpreted page picture
currentInterpPicture :: ViewerState -> Picture
currentInterpPicture st =
  getPageAt
    (currentTestPages st)
    (pageIndex st)
    (paperCanvas (errorPlaceholder (currentTestName st) "No pages generated"))

-- | Access the active reference page picture
currentRefPicture :: ViewerState -> Picture
currentRefPicture st =
  getPageAt
    (referencePages st)
    (pageIndex st)
    (paperCanvas (missingImagePlaceholder (currentTestName st)))

-- | Loading placeholder displayed while background worker computes
loadingPlaceholder :: String -> Maybe String -> Picture
loadingPlaceholder name mEst = case mEst of
  Just est ->
    Pictures
      [ Translate (-180) 50 $
          Color (makeColor 1.0 0.8 0.2 1.0) $
            Scale 0.16 0.16 $
              Text "Interpreting Script...",
        Translate (-220) 10 $
          Color (makeColor 1.0 0.45 0.2 1.0) $
            Scale 0.12 0.12 $
              Text "WARNING: Computationally intensive test!",
        Translate (-200) (-20) $
          Color (makeColor 1.0 0.85 0.35 1.0) $
            Scale 0.12 0.12 $
              Text ("Expected duration: " ++ est ++ " - Please wait..."),
        Translate (-120) (-60) $
          Color (greyN 0.5) $
            Scale 0.10 0.10 $
              Text ("(" ++ name ++ ")")
      ]
  Nothing ->
    Pictures
      [ Translate (-140) 20 $
          Color (makeColor 0.2 0.6 1.0 1.0) $
            Scale 0.14 0.14 $
              Text "Interpreting Script...",
        Translate (-120) (-20) $
          Color (greyN 0.4) $
            Scale 0.10 0.10 $
              Text ("(" ++ name ++ ")")
      ]

-- | Error placeholder displayed if script execution fails
errorPlaceholder :: String -> String -> Picture
errorPlaceholder name err =
  Pictures
    [ Translate (-140) 40 $ Color red $ Scale 0.14 0.14 $ Text "Execution Error:",
      Translate (-180) 0 $ Color (greyN 0.2) $ Scale 0.09 0.09 $ Text (take 45 err),
      Translate (-120) (-40) $ Color (greyN 0.5) $ Scale 0.09 0.09 $ Text ("(" ++ name ++ ")")
    ]

-- Helper accessors
currentCase :: ViewerState -> TestCase
currentCase st = testCases st !! activeIndex st

currentTestName :: ViewerState -> String
currentTestName st = testName (currentCase st)

currentImageLabel :: ViewerState -> String
currentImageLabel st = case refImagePaths (currentCase st) of
  [] -> "None"
  paths@(firstP : _) ->
    let idx = pageIndex st
        p = if idx < length paths then paths !! idx else firstP
        pageInfo =
          if length paths > 1
            then " (Page " ++ show (idx + 1) ++ ")"
            else ""
     in takeFileName p ++ pageInfo

currentInterpLabel :: ViewerState -> String
currentInterpLabel st =
  let base = takeFileName (psFilePath (currentCase st))
      pageCount = length (currentTestPages st)
      pageInfo =
        if pageCount > 1
          then " (Page " ++ show (pageIndex st + 1) ++ ")"
          else ""
   in base ++ pageInfo
