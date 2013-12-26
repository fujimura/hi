module Hi
  (
    run
  , process
  ) where

import           Hi.Directory        (inDirectory)
import           Hi.FilePath         (rewritePath)
import           Hi.Template         (readTemplates)
import           Hi.Types
import           Hi.Utils

import           Control.Applicative
import           Control.Monad
import           Data.List           (isSuffixOf)
import           Data.Maybe          (fromJust)
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as LT
import           Data.Text.Template  (Context, substitute)
import           System.Directory    (createDirectoryIfMissing)
import           System.FilePath     (dropFileName)
import           System.Process      (system)

-- | Run 'hi'.
run :: [Option] -> IO ()
run options = do
    putStrLn $ "Creating new project from repository: " ++ repository
    writeFiles =<< showFileList =<< process options <$> readTemplates repository
    postProcess options
  where
    repository = fromJust $ lookupArg "repository" options

-- |Write given 'Files' to filesystem.
writeFiles :: Files -> IO ()
writeFiles = mapM_ (uncurry write)
  where
    write :: FilePath -> String -> IO ()
    write path content = createDirectoryIfMissing True (dropFileName path) >> writeFile path content

-- | Show 'Files' to stdout.
showFileList :: Files -> IO Files
showFileList files = do
    mapM_ (showFile . fst) files
    return files
  where
    showFile :: FilePath -> IO ()
    showFile path = putStrLn $ "    " ++ green "create" ++ "  " ++ path
    green :: String -> String
    green x = "\x1b[32m" ++ x ++ "\x1b[0m"

-- |Process given 'Files' and return result. it does
--
-- 1. rewrite path
--
-- 2. substitute arguments
process :: [Option] -> Files -> Files
process options files = map go $ filter (isTemplate . fst) files
  where
    isTemplate path    = ".template" `isSuffixOf` path
    go (path, content) = (rewritePath options path, substitute' content)
    substitute' text   = LT.unpack $ substitute (T.pack text) (context options)

-- | Return 'Context' obtained by given 'Options'
context :: [Option] -> Context
context options x = T.pack (fromJust $ lookup (T.unpack x) [(k,v) | (Arg k v) <- options])

postProcess :: [Option] -> IO ()
postProcess options = do
    when (InitializeGitRepository `elem` options) $
      -- TODO This wont' work unless template has `package-name` as root dir.
      inDirectory (fromJust $ lookupArg "packageName" options) $
        void $ system "git init && git add . && git commit -m \"Initial commit\""
    return ()
