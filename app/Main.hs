module Main (main) where

import Control.Monad (forM_)
import PostScript
  ( executeScript,
    graphicCommands,
  )
import PostScript.Primitives.Graphics.Render
  ( renderAllPagesToSVG,
  )
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath
  ( dropExtension,
    splitFileName,
    takeBaseName,
    takeExtension,
    (</>),
  )

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Just (path, mOut, mPage) -> run path mOut mPage
    Nothing -> do
      putStrLn "Usage: postscript-interpreter <file.ps> [-o <out_path_or_dir>] [--page <n>]"
      exitFailure

parseArgs :: [String] -> Maybe (FilePath, Maybe FilePath, Maybe Int)
parseArgs = go Nothing Nothing Nothing
  where
    go mPath mOut mPage [] = case mPath of
      Just p  -> Just (p, mOut, mPage)
      Nothing -> Nothing
    go mPath _ mPage ("-o" : out : rest) =
      go mPath (Just out) mPage rest
    go mPath mOut _ ("--page" : pStr : rest) =
      case reads pStr of
        [(n, "")] -> go mPath mOut (Just n) rest
        _         -> Nothing
    go Nothing mOut mPage (arg : rest) =
      go (Just arg) mOut mPage rest
    go mPath Nothing mPage (arg : rest) =
      go mPath (Just arg) mPage rest
    go _ _ _ _ = Nothing

run :: FilePath -> Maybe FilePath -> Maybe Int -> IO ()
run path mOut mPage = do
  content <- readFile path
  res <- executeScript content
  case res of
    Left err -> putStrLn ("Error: " ++ err) >> exitFailure
    Right st -> do
      let allCmds = reverse (graphicCommands st)
          pages = renderAllPagesToSVG 612 792 allCmds
          total = length pages
      if total == 0
        then pure ()
        else case mPage of
          Just pNum ->
            if pNum >= 1 && pNum <= total
              then case mOut of
                Just outPath -> writeFile outPath (pages !! (pNum - 1))
                Nothing      -> putStr (pages !! (pNum - 1))
              else do
                putStrLn $ "Error: Page " ++ show pNum ++ " out of range (1.." ++ show total ++ ")"
                exitFailure
          Nothing -> case mOut of
            Nothing ->
              case pages of
                (firstPage : _) -> putStr firstPage
                []              -> pure ()
            Just outTarget ->
              writePages outTarget (takeBaseName path) pages

writePages :: FilePath -> String -> [String] -> IO ()
writePages _ _ [] = pure ()
writePages target baseName [singlePage] = do
  isDir <- doesDirectoryExist target
  let finalPath =
        if isDir || takeExtension target /= ".svg"
          then target </> (baseName ++ ".svg")
          else target
      (dir, _) = splitFileName finalPath
  createDirectoryIfMissing True dir
  writeFile finalPath singlePage
writePages target baseName pages@(firstPage : _) = do
  isDir <- doesDirectoryExist target
  let isSvgFile = not isDir && takeExtension target == ".svg"
      outBase = if isSvgFile then dropExtension target else target </> baseName
      (dir, _) = splitFileName outBase
  createDirectoryIfMissing True dir
  forM_ (zip [1 :: Int ..] pages) $ \(idx, pageSvg) -> do
    let pagePath = outBase ++ "_" ++ show idx ++ ".svg"
    writeFile pagePath pageSvg
  -- Also write the base target as page 1 if a specific .svg filename was requested
  if isSvgFile
    then writeFile target firstPage
    else pure ()
