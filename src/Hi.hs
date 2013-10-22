{-# LANGUAGE NamedFieldPuns #-}
module Hi
  (
    run
  ) where

import           Hi.Context          (context)
import           Hi.FilePath         (rewritePath)
import           Hi.Template         (readTemplates)
import           Hi.Types
import           Control.Applicative
import           System.Directory    (createDirectoryIfMissing)
import           System.FilePath     (dropFileName)

import qualified Data.Text           as T
import qualified Data.Text.Lazy      as LT
import           Data.Text.Template  (substitute)

writeFiles :: Files -> IO ()
writeFiles = mapM_ (uncurry write)
  where
    write :: FilePath -> String -> IO ()
    write path content = do
      createDirectoryIfMissing True $ dropFileName path
      writeFile path content

process :: InitFlags -> Files -> Files
process initFlags = map go
  where
    go (path, content) = (rewritePath initFlags path, substitute' content)
    substitute' t      = LT.unpack $ substitute (T.pack t) (context initFlags)

showFileList :: Files -> IO Files
showFileList files = do
    mapM_ (showFile . fst) files
    return files
  where
    showFile :: FilePath -> IO ()
    showFile path = putStrLn $ "    " ++ green "create" ++ "  " ++ path

green :: String -> String
green x = "\x1b[32m" ++ x ++ "\x1b[0m"

createFileList :: InitFlags -> IO Files
createFileList initFlags@(InitFlags {repository}) =
    process initFlags <$> readTemplates repository

run :: InitFlags -> IO ()
run initFlags@(InitFlags {repository}) = do
    putStrLn $ "Creating new project from repository: " ++ repository
    writeFiles =<< showFileList =<< createFileList initFlags
