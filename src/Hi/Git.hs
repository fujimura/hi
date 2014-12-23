module Hi.Git
    (
      clone
    , config
    , lsFiles
    , expandUrl
    ) where

import           Control.Applicative
import           System.Exit
import           System.Process      (readProcess, readProcessWithExitCode, system)

expandUrl :: String -> String
expandUrl ('g':'h':':':xs) = "git@github.com:" ++ xs ++ ".git"
expandUrl xs = xs

-- | Clone given repository to current directory
clone :: String -> IO ExitCode
clone repoUrl = do
    _ <- system $ "git clone --no-checkout --quiet " ++ repoUrl ++ " " ++ "./"
    system "git checkout HEAD --quiet"

-- | Return file list by `git ls-files`
lsFiles :: IO [String]
lsFiles = lines <$> readProcess "git" ["ls-files"] []

-- | Return given config value
config :: String -> IO (Maybe String)
config name = do
    (exitCode,s,_) <- readProcessWithExitCode "git" ["config", name] []
    return $ if exitCode == ExitSuccess
               then Just $ removeNewline s
               else Nothing

removeNewline :: String -> String
removeNewline = reverse . dropWhile (=='\n') . reverse
