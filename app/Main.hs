module Main (main) where

import PostScript (canvasElements, executeScript)
import PostScript.Primitives.Graphics.Render (generateSVGDocument)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      content <- readFile path
      res <- executeScript content
      case res of
        Left err -> putStrLn ("Error: " ++ err) >> exitFailure
        Right st -> do
          let elems = reverse (canvasElements st)
          if not (null elems)
            then putStr (generateSVGDocument 612 792 elems)
            else pure ()
    _ -> putStrLn "Usage: postscript-interpreter <file.ps>"
