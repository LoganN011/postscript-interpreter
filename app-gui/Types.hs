module Types where

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Graphics.Gloss.Data.Picture (Picture)

-- | View mode for the canvas
data ViewMode
  = -- | Dual view: Reference on left, Interpreted on right
    ViewSideBySide
  | -- | Full canvas interpreted PostScript
    ViewInterpretedOnly
  | -- | Full canvas reference image
    ViewReferenceOnly
  deriving (Show, Eq)

-- | A single test file entry
data TestCase = TestCase
  { testName :: String,
    psFilePath :: FilePath,
    -- | Discovered reference images per page
    refImagePaths :: [FilePath]
  }
  deriving (Show, Eq)

-- | Interactive GUI viewer state
data ViewerState = ViewerState
  { testCases :: [TestCase],
    activeIndex :: Int,
    viewMode :: ViewMode,
    currentTestPages :: [Picture],
    pageIndex :: Int,
    -- | Pre-loaded reference pictures for each page
    referencePages :: [Picture],
    panOffset :: (Float, Float),
    zoomScale :: Float,
    isDragging :: Bool,
    dragAnchor :: (Float, Float),
    showMenuList :: Bool,
    menuScroll :: Int,
    statusMessage :: String,
    currentJobId :: Int,
    activeWorkerTid :: Maybe ThreadId,
    asyncMVar :: MVar (Int, Either String [Picture], String)
  }
