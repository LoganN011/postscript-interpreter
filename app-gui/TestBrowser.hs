module TestBrowser
  ( discoverTestCases,
    loadTestExecution,
  )
where

import Control.Exception (SomeException, catch)
import Control.Monad (filterM)
import Data.Char (toLower)
import Data.List (nubBy, sortOn)
import PostScript (executeScript)
import PostScript.Types (PSState)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    findExecutable,
    listDirectory,
  )
import System.Exit (ExitCode (..))
import System.FilePath
  ( dropExtension,
    takeBaseName,
    takeExtension,
    (</>),
  )
import System.Process (readProcessWithExitCode)
import Types (TestCase (..))

-- | Supported image extensions for reference images
imageExtensions :: [String]
imageExtensions = [".jpg", ".jpeg", ".png", ".bmp"]

-- | Discover test cases from a specified target or standard fallback directories
discoverTestCases :: Maybe FilePath -> IO [TestCase]
discoverTestCases mTarget = case mTarget of
  Just path -> do
    isFile <- doesFileExist path
    if isFile
      then do
        tc <- makeTestCase path
        pure [tc]
      else do
        isDir <- doesDirectoryExist path
        if isDir
          then scanDirectory path
          else pure []
  Nothing -> do
    let defaultDirs =
          [ "test/working_graphics",
            "test/Examples",
            "Project/postscript-interpreter/test/working_graphics",
            "Project/postscript-interpreter/test/Examples"
          ]
    existingDirs <- filterM doesDirectoryExist defaultDirs
    results <- concat <$> mapM scanDirectoryRecursively existingDirs
    let uniqueCases = nubBy (\a b -> testName a == testName b) results
    pure (sortOn testName uniqueCases)

-- | Scan a directory (non-recursive) for .ps files
scanDirectory :: FilePath -> IO [TestCase]
scanDirectory dir = do
  entries <- listDirectory dir
  let psFiles = filter (\f -> map toLower (takeExtension f) == ".ps") entries
  cases <- mapM (makeTestCase . (dir </>)) psFiles
  pure (sortOn testName cases)

-- | Scan a directory and subdirectories recursively for .ps files
scanDirectoryRecursively :: FilePath -> IO [TestCase]
scanDirectoryRecursively dir = do
  entries <- listDirectory dir
  let fullPaths = map (dir </>) entries
  files <- filterM doesFileExist fullPaths
  subdirs <- filterM doesDirectoryExist fullPaths
  let psFiles = filter (\f -> map toLower (takeExtension f) == ".ps") files
  directCases <- mapM makeTestCase psFiles
  nestedCases <- concat <$> mapM scanDirectoryRecursively subdirs
  pure (directCases ++ nestedCases)

-- | Form a TestCase given a .ps file path, discovering matching sister image(s) for all pages
makeTestCase :: FilePath -> IO TestCase
makeTestCase psPath = do
  let name = takeBaseName psPath
  images <- findPageImages psPath
  pure
    TestCase
      { testName = name,
        psFilePath = psPath,
        refImagePaths = images
      }

-- | Discovers reference images for all pages of a document
findPageImages :: FilePath -> IO [FilePath]
findPageImages psPath = do
  let baseWithoutExt = dropExtension psPath
  -- 1. Check for multi-page images: <base>_1.jpg, <base>_2.jpg, <base>-1.jpg, etc.
  numbered <- findNumberedImages baseWithoutExt 1
  if not (null numbered)
    then pure numbered
    else do
      -- 2. Check for single base image: <base>.jpg, <base>.png, etc.
      mSingle <- findFirstExisting [baseWithoutExt ++ ext | ext <- imageExtensions]
      case mSingle of
        Just single -> pure [single]
        Nothing -> tryRenderWithGhostscript psPath

-- | Find sequence of numbered images: base_1, base_2, ... up to 20
findNumberedImages :: FilePath -> Int -> IO [FilePath]
findNumberedImages base pageNum
  | pageNum > 20 = pure []
  | otherwise = do
      let nStr = show pageNum
          candidates =
            [ base ++ sep ++ nStr ++ ext
            | sep <- ["_", "-", ".", "_page", "-page"],
              ext <- imageExtensions
            ]
      mFound <- findFirstExisting candidates
      case mFound of
        Just path -> do
          rest <- findNumberedImages base (pageNum + 1)
          pure (path : rest)
        Nothing ->
          -- Stop at first missing page if pageNum > 1
          pure []

-- | If no reference images exist on disk, render pages using Ghostscript if available
tryRenderWithGhostscript :: FilePath -> IO [FilePath]
tryRenderWithGhostscript psPath = do
  mGs <- findExecutable "gs"
  case mGs of
    Nothing -> pure []
    Just gsBin -> do
      let name = takeBaseName psPath
          cacheDir = "/tmp/postscript_gui_cache" </> name
      createDirectoryIfMissing True cacheDir
      let outPattern = cacheDir </> "page_%d.jpg"
          args =
            [ "-sDEVICE=jpeg",
              "-r150",
              "-sOutputFile=" ++ outPattern,
              "-dNOPAUSE",
              "-dBATCH",
              psPath
            ]
      (exitCode, _, _) <- readProcessWithExitCode gsBin args ""
      case exitCode of
        ExitSuccess -> findNumberedImages (cacheDir </> "page") 1
        _ -> pure []

findFirstExisting :: [FilePath] -> IO (Maybe FilePath)
findFirstExisting [] = pure Nothing
findFirstExisting (candidate : rest) = do
  exists <- doesFileExist candidate
  if exists
    then pure (Just candidate)
    else findFirstExisting rest

-- | Executes the .ps file at path and returns the executed state or error string
loadTestExecution :: FilePath -> IO (Either String PSState)
loadTestExecution path = do
  eContent <- (Right <$> readFile path) `catch` (\(ex :: SomeException) -> pure (Left (show ex)))
  case eContent of
    Left err -> pure (Left err)
    Right content -> executeScript content
