module Hi.Git
    (
      expandUrl
    ) where

import           Hi.Types
import           Hi.Utils

import           Data.List           (isPrefixOf)

expandUrl :: String -> String
expandUrl url = if "gh:" `isPrefixOf` url
                  then expand url
                  else url
  where expand (_:_:_:xs) = "git@github.com:" ++ xs ++ ".git"
