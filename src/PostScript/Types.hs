{- ============================================================================
   MODULE: PostScript.Types
   PATH:   src/PostScript/Types.hs

   DESCRIPTION:
     Central data definitions for the PostScript interpreter. Defines runtime
     PostScript objects, global state structures, 2D transformation matrices,
     path primitives, and the `Interpreter` monad transformer stack.

   RESPONSIBILITIES:
     1. Define Runtime Objects (`Object` data type):
        - `PSInteger Int`, `PSReal Double`, `PSBoolean Bool`
        - `PSString String`, `PSName String` (literal names e.g. `/x`)
        - `PSSymbol String` (executable names e.g. `add`, `def`)
        - `PSBlock [Object]` (deferred execution blocks `{ ... }`)
        - `PSDict (Map String Object)`
     2. Define Matrix & Transformation Types:
        - `Matrix (Double, Double, Double, Double, Double, Double)` [a b c d tx ty]
     3. Define Path & Graphics Geometry Types:
        - `PathSegment`: `MoveTo x y`, `LineTo x y`, `Arc x y r ang1 ang2`, `ClosePath`
     4. Define Graphics State (`GraphicsState`):
        - Current Transformation Matrix (CTM)
        - Current vector path `[PathSegment]` and current position `Maybe (Double, Double)`
        - Line attributes: `lineWidth`, line join, line cap
        - Color attributes: `rgbColor :: (Double, Double, Double)`
     5. Define Global Interpreter State (`PSState`):
        - `operandStack :: [Object]`
        - `dictStack    :: [Map String Object]`
        - `gState       :: GraphicsState`
        - `gStateStack  :: [GraphicsState]` (for `gsave`/`grestore`)
     6. Define Execution Monad:
        - `type Interpreter a = StateT PSState IO a`

   FUNCTIONS / INITIALIZERS TO IMPLEMENT:
     - initGraphicsState :: GraphicsState
         Default CTM identity matrix, empty path, black stroke, default line width.
     - initPSState :: PSState
         Default empty stacks, root system dictionary initialized, default graphics state.
   ============================================================================ -}

module PostScript.Types where

import Control.Monad.State (StateT)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- Core PostScript Data Object
data Object
  = PSInteger Int
  | PSReal Double
  | PSBoolean Bool
  | PSString String
  | PSName String
  | PSSymbol String
  | PSBlock [Object]
  | PSDict (Map String Object)
  deriving (Show, Eq)

-- 2D Affine Transformation Matrix
type Matrix = (Double, Double, Double, Double, Double, Double)

identityMatrix :: Matrix
identityMatrix = (1.0, 0.0, 0.0, 1.0, 0.0, 0.0)

-- Vector path segments
data PathSegment
  = MoveTo Double Double
  | LineTo Double Double
  | CurveTo Double Double Double Double Double Double
  | Arc Double Double Double Double Double
  | ArcN Double Double Double Double Double
  | ClosePath
  deriving (Show, Eq)

-- Drawing and visual styling state
data GraphicsState = GraphicsState
  { ctm :: Matrix,
    currentPath :: [PathSegment],
    currentPoint :: Maybe (Double, Double),
    rgbColor :: (Double, Double, Double),
    lineWidth :: Double,
    lineCap :: Int,
    lineJoin :: Int,
    dashArray :: [Double],
    dashOffset :: Double,
    fontName :: String,
    fontSize :: Double
  }
  deriving (Show, Eq)

-- Top-level global interpreter execution state
data PSState = PSState
  { operandStack :: [Object],
    dictStack :: [Map String Object],
    gState :: GraphicsState,
    gStateStack :: [GraphicsState],
    canvasElements :: [String]
  }
  deriving (Show)

-- Interpreter monad stack
type Interpreter a = StateT PSState IO a

-- Default initial graphics state
initGraphicsState :: GraphicsState
initGraphicsState =
  GraphicsState
    { ctm = identityMatrix,
      currentPath = [],
      currentPoint = Nothing,
      rgbColor = (0.0, 0.0, 0.0),
      lineWidth = 1.0,
      lineCap = 0,
      lineJoin = 0,
      dashArray = [],
      dashOffset = 0.0,
      fontName = "Helvetica",
      fontSize = 10.0
    }

-- Default initial interpreter state
initPSState :: PSState
initPSState =
  PSState
    { operandStack = [],
      dictStack = [Map.empty],
      gState = initGraphicsState,
      gStateStack = [],
      canvasElements = []
    }
