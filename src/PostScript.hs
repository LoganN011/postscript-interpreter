{- ============================================================================
   MODULE: PostScript
   PATH:   src/PostScript.hs

   DESCRIPTION:
     High-level public API wrapper module. Re-exports the core execution functions
     and primary data structures so external executables, tests, or library consumers
     only need to import `PostScript`.

   RESPONSIBILITIES:
     1. Re-export Core Monad & Types (`Object`, `PSState`, `Interpreter`).
     2. Re-export Parsing API (`parsePostScript`).
     3. Re-export Evaluation API (`evalProgram`, `runInterpreter`, `initPSState`).
     4. Expose high-level entry helper functions for running scripts directly from
        Haskell code without touching lower-level internals.

   FUNCTIONS / RE-EXPORTS TO IMPLEMENT:
     - module PostScript.Types
     - module PostScript.Lexer
     - module PostScript.Evaluator
     - executeScript :: String -> IO (Either String PSState)
         High-level helper to execute raw PS string from initial state.
   ============================================================================ -}

module PostScript
  ( module PostScript.Types,
    module PostScript.Lexer,
    module PostScript.Evaluator,
    executeScript,
  )
where

import PostScript.Evaluator
import PostScript.Lexer
import PostScript.Types

-- Executes a raw PostScript source string from a default initial interpreter state
executeScript :: String -> IO (Either String PSState)
executeScript = executeScriptWith (initPSState { dictStack = [defaultSystemDict] })

-- Executes a raw PostScript source string starting from a provided initial 'PSState'.
executeScriptWith :: PSState -> String -> IO (Either String PSState)
executeScriptWith s0 input = case parsePostScript input of
  Left err   -> return (Left ("Parse error: " ++ err))
  Right objs -> runInterpreter objs s0