{- ============================================================================
   MODULE: PostScript.Lexer
   PATH:   src/PostScript/Lexer.hs

   DESCRIPTION:
     Tokenizer and block structural parser built with Megaparsec (or Attoparsec).
     Converts raw PostScript string input into a flat list of executable `[Object]` items,
     handling comments, whitespace, numbers, literal names, and nested `{ ... }` blocks.

   RESPONSIBILITIES:
     1. Lexical Analysis & Whitespace Handling:
        - Strip PostScript comments (lines starting with `%`).
        - Treat all standard space, tab, newline, and carriage return characters as delimiters.
     2. Parse Literals and Numbers:
        - Parse integers (e.g. `42`, `-10`).
        - Parse floating-point numbers (e.g. `3.14159`, `-.05`).
        - Parse literal names prefixed with slash `/` (e.g. `/x`, `/myProc` -> `PSName "x"`).
        - Parse executable symbols without slash (e.g. `add`, `moveto`, `dup` -> `PSSymbol "add"`).
        - Parse string literals inside parentheses `(hello world)` -> `PSString`.
     3. Parse Deferred Code Blocks:
        - Recursively parse executable blocks enclosed in curly braces `{ ... }` into `PSBlock [Object]`.
        - Ensure proper nesting validation for curly braces.

   FUNCTIONS TO IMPLEMENT:
     - parsePostScript :: String -> Either String [Object]
         Top-level parser runner over source text.
     - sc :: Parser ()
         Space consumer (handles spaces, newlines, and comments starting with `%`).
     - pObject :: Parser Object
         Main object dispatcher trying integers, reals, names, symbols, strings, and blocks.
     - pInteger :: Parser Object
     - pReal :: Parser Object
     - pName :: Parser Object (starts with `/`)
     - pSymbol :: Parser Object (identifiers without `/`)
     - pString :: Parser Object (parenthesized strings)
     - pBlock :: Parser Object (recursive `{ ... }` parsing)
   ============================================================================ -}