module Main (main) where

import PostScript (executeScript)
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
        Right _  -> pure ()
    _ -> putStrLn "Usage: postscript-interpreter <file.ps>"
