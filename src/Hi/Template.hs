module Hi.Template
    (
      readTemplates
    , untemplate
    ) where

import           Hi.Directory         (inTemporaryDirectory)
import           Hi.Types
import           Control.Applicative
import           Data.List.Split      (splitOn)
import           System.Exit          (ExitCode)
import           System.Process       (system, readProcess)

-- | Read templates in given 'FilePath'
readTemplates :: FilePath -> IO Files
readTemplates repo =
    inTemporaryDirectory "hi" $ do
        -- TODO Handle error
        _ <- cloneRepo repo
        paths <- lsFiles
        contents <- mapM readFile paths
        return $ zip paths contents

-- | Remove \".template\" from 'FilePath'
untemplate :: FilePath -> FilePath
untemplate = head . splitOn ".template"

-- | Clone given repository to current directory
cloneRepo :: String -> IO ExitCode
cloneRepo repoUrl = do
    _ <- system $ "git clone --no-checkout --quiet --depth=1 " ++ repoUrl ++ " " ++ "./"
    system "git checkout HEAD --quiet"

-- | Return file list by `git ls-files`
lsFiles :: IO [String]
lsFiles = lines <$> readProcess "git" ["ls-files"] []
