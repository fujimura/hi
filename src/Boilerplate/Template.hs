module Boilerplate.Template
    (
      withTemplatesFromRepo
    , untemplate
    ) where

import           Boilerplate.Directory (inTemporaryDirectory)
import           Data.List.Split       (splitOn)
import           System.Exit           (ExitCode)
import           System.FilePath.Glob  (compile, globDir1)
import           System.Process        (system)

withTemplatesFromRepo :: String -> ([FilePath] -> IO a) -> IO a
withTemplatesFromRepo repo cb =
    inTemporaryDirectory "boilerplate" $ do
        _ <- cloneRepo repo
        paths <- globDir1 (compile "./**/*.template") "./"
        cb paths

untemplate :: FilePath -> FilePath
untemplate = head . splitOn ".template"

cloneRepo :: String -> IO ExitCode
cloneRepo repoUrl = do
    system $ "git clone --no-checkout --quiet --depth=1 " ++ repoUrl ++ " ./"
    system "git checkout HEAD --quiet"
