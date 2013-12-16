module Hi
  (
    run
  , process
  ) where

import           Hi.FilePath         (rewritePath)
import           Hi.Template         (readTemplates)
import           Hi.Types

import           Control.Applicative
import           Data.List           (isSuffixOf)
import           Data.Maybe          (fromJust)
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as LT
import           Data.Text.Template  (Context, substitute)
import           System.Directory    (createDirectoryIfMissing)
import           System.FilePath     (dropFileName)

-- | Run 'hi'.
run :: InitFlags -> IO ()
run initFlags = do
    putStrLn $ "Creating new project from repository: " ++ repository
    writeFiles =<< showFileList =<< process initFlags <$> readTemplates repository
  where
    repository = fromJust $ lookup "repository" initFlags

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
process :: InitFlags -> Files -> Files
process initFlags files = map go $ filter (isTemplate . fst) files
  where
    isTemplate path    = ".template" `isSuffixOf` path
    go (path, content) = (rewritePath initFlags path, substitute' content)
    substitute' text   = LT.unpack $ substitute (T.pack text) (context initFlags)

-- | Return 'Context' obtained by given 'InitFlags'
context :: InitFlags -> Context
context flags x = T.pack (fromJust $ lookup (T.unpack x) flags)
