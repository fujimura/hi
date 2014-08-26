module Hi.Git
    (
      clone
    , lsFiles
    , expandUrl
    ) where

import           Control.Applicative
import           Data.List           (isPrefixOf)
import           System.Exit         (ExitCode)
import           System.Process      (readProcess, system)

expandUrl :: String -> String
expandUrl url = if "gh:" `isPrefixOf` url
                  then expand url
                  else url
  where expand (_:_:_:xs) = "git@github.com:" ++ xs ++ ".git"

-- | Clone given repository to current directory
clone :: String -> IO ExitCode
clone repoUrl = do
    _ <- system $ "git clone --no-checkout --quiet --depth=1 " ++ repoUrl ++ " " ++ "./"
    system "git checkout HEAD --quiet"

-- | Return file list by `git ls-files`
lsFiles :: IO [String]
lsFiles = lines <$> readProcess "git" ["ls-files"] []
