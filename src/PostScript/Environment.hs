{- ============================================================================
   MODULE: PostScript.Environment
   PATH:   src/PostScript/Environment.hs

   DESCRIPTION:
     Low-level internal monadic helper infrastructure. Manipulates `PSState` record 
     fields safely inside the `Interpreter` monad. Provides stack safety checks 
     and dictionary scoping lookups for built-in primitive modules.

   RESPONSIBILITIES:
     1. Operand Stack Mutation:
        - `push`: Push object onto top of `operandStack`.
        - `pop`: Pop object off `operandStack` safely (throws error on stack underflow).
        - `peek`: Inspect top object without popping.
        - `popTwo`: Helper to pop top two arguments `(b, a)` for binary operations.
     2. Dictionary Stack Operations:
        - `lookupDict`: Search `dictStack` from top dictionary down to root dictionary.
        - `defineSymbol`: Bind key-value pair in top-most dictionary (`def` mechanics).
        - `pushDict`: Push map onto `dictStack` (`begin` mechanics).
        - `popDict`: Pop map off `dictStack` (`end` mechanics).
     3. Graphics State Helpers:
        - `getGState`: Retrieve active `GraphicsState`.
        - `modifyGState`: Apply pure update function to active `GraphicsState`.

   FUNCTIONS TO IMPLEMENT:
     - push :: Object -> Interpreter ()
     - pop :: Interpreter Object
     - peek :: Interpreter Object
     - popTwo :: Interpreter (Object, Object)
     - lookupDict :: String -> Interpreter (Maybe Object)
     - defineSymbol :: String -> Object -> Interpreter ()
     - pushDict :: Map String Object -> Interpreter ()
     - popDict :: Interpreter (Map String Object)
     - getGState :: Interpreter GraphicsState
     - modifyGState :: (GraphicsState -> GraphicsState) -> Interpreter ()
   ============================================================================ -}