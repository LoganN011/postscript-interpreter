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