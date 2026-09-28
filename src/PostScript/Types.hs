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