{- ============================================================================
   MODULE: Main
   PATH:   app/Main.hs

   DESCRIPTION:
     Executable entry point and Command Line Interface (CLI) driver loop for 
     the PostScript interpreter. Handles argument parsing, file I/O, engine
     initialization, and top-level error reporting.

   RESPONSIBILITIES:
     1. Parse Command Line Arguments:
        - Accept paths to `.ps` source files (e.g., `cabal run postscript-interpreter -- input.ps`).
        - Handle optional CLI flags (e.g., `-o output.svg` for specifying output file paths).
     2. Read Input Files:
        - Read raw PostScript source text safely from disk.
     3. Invoke Pipeline:
        - Call `PostScript.Lexer.parsePostScript` on raw source text.
        - Pass parsed `[Object]` stream to `PostScript.Evaluator.runInterpreter`.
     4. Output Management & Error Handling:
        - Catch parsing errors (syntax errors, unmatched braces) and print clear messages.
        - Catch runtime evaluation errors (stack underflow, type mismatch, undefined symbols).
        - Save generated SVG string from `PSState` to target `.svg` file upon completion.

   FUNCTIONS TO IMPLEMENT:
     - main :: IO ()
         Reads CLI args, parses file, initializes default state, and triggers evaluator.
     - runFile :: FilePath -> FilePath -> IO ()
         Handles reading input file, executing pipeline, and writing SVG output.
     - parseArgs :: [String] -> Either String (FilePath, FilePath)
         Extracts input/output file paths from argument list.
   ============================================================================ -}
module Main (main) where

main :: IO ()
main = putStrLn "Hello, Haskell!"
