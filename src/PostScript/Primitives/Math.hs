{- ============================================================================
   MODULE: PostScript.Primitives.Math
   PATH:   src/PostScript/Primitives/Math.hs

   DESCRIPTION:
     Implements PostScript mathematical, arithmetic, and trigonometric primitives.

   RESPONSIBILITIES:
     1. Basic Arithmetic Operators:
        - `add`: Pop two numbers, push sum (supports both Int and Real promotion).
        - `sub`: Pop two numbers (b, a), push difference (a - b).
        - `mul`: Pop two numbers, push product.
        - `div`: Pop two numbers (b, a), push real division result (a / b).
        - `idiv`: Integer division (a `div` b).
        - `mod`: Integer modulo (a `mod` b).
     2. Numeric Manipulation:
        - `neg`: Negate top number.
        - `abs`: Absolute value of top number.
        - `ceiling`, `floor`, `round`, `truncate`.
     3. Trigonometric & Exponential Operators:
        - `sqrt`: Square root.
        - `atan`: Arctangent (atan2 y x).
        - `cos`, `sin`: Cosine and sine (input in degrees).
        - `exp`: Exponentiation ($base^{exponent}$).
        - `ln`, `log`: Natural and base-10 logarithms.

   FUNCTIONS TO IMPLEMENT:
     - psAdd, psSub, psMul, psDiv, psIdiv, psMod :: Interpreter ()
     - psNeg, psAbs, psCeil, psFloor, psRound, psTrunc :: Interpreter ()
     - psSqrt, psAtan, psCos, psSin, psExp, psLn, psLog :: Interpreter ()
   ============================================================================ -}