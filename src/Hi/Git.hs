module Hi.Git
    (
      clone
    , lsFiles
    , expandUrl
    ) where

import           Control.Applicative
import           System.Exit         (ExitCode)
import           System.Process      (readProcess, system)

expandUrl :: String -> String
expandUrl url = case url of
  'g':'h':':':repo -> "git@github.com:" ++ repo ++ ".git"
  _ -> url

-- | Clone given repository to current directory
clone :: String -> IO ExitCode
clone repoUrl = do
    _ <- system $ "git clone --no-checkout --quiet --depth=1 " ++ repoUrl ++ " " ++ "./"
    system "git checkout HEAD --quiet"

-- | Return file list by `git ls-files`
lsFiles :: IO [String]
lsFiles = lines <$> readProcess "git" ["ls-files"] []
