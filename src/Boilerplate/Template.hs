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

-- | Run callback with list of template files.
withTemplatesFromRepo :: String               -- ^ Repository url
                      -> ([FilePath] -> IO a) -- ^ Callback which takes list of the template file
                      -> IO a                 -- ^ Result
withTemplatesFromRepo repo cb =
    inTemporaryDirectory "boilerplate" $ do
        _ <- cloneRepo repo
        paths <- globDir1 (compile "./**/*.template") "./"
        cb paths

-- | Remove \".template\" from 'FilePath'
untemplate :: FilePath -> FilePath
untemplate = head . splitOn ".template"

-- | Clone given repository to current directory
cloneRepo :: String -> IO ExitCode
cloneRepo repoUrl = do
    system $ "git clone --no-checkout --quiet --depth=1 " ++ repoUrl ++ " ./"
    system "git checkout HEAD --quiet"
