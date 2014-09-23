{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Hi.Option
    (
      defaultRepo
    , buildOption
    ) where

import           Hi.CommandLineOption (CommandLineOption)
import qualified Hi.CommandLineOption as CommandLineOption
import qualified Hi.Git               as Git
import           Hi.Types

import           Control.Applicative
import           Data.Char            (isUpper, toLower)
import           Data.Maybe           (fromMaybe)
import           Data.Time.Calendar   (toGregorian)
import           Data.Time.Clock      (getCurrentTime, utctDay)

buildOption :: CommandLineOption -> IO Option
buildOption copt = do
    let packageName = (removeDup . hyphenize) cModuleName
    year <- getCurrentYear
    -- TODO Raise error if author/email is not provided from git config or
    -- commandline options
    author <- guessAuthor
    email <- guessEmail
    return  Option { initializeGitRepository = fromMaybe False $ CommandLineOption.initializeGitRepository copt
                    , moduleName     = cModuleName
                    , packageName    = fromMaybe packageName $ CommandLineOption.packageName copt
                    , author         = fromMaybe author $ CommandLineOption.author copt
                    , email          = fromMaybe email $ CommandLineOption.email copt
                    , templateSource = guessTemplate
                    , year           = year
                    }
  where
    cModuleName = CommandLineOption.moduleName copt
    removeDup []           = []
    removeDup [x]          = [x]
    removeDup ('-':'-':xs) = removeDup('-':xs)
    removeDup (x:xs)       = x: removeDup xs
    hyphenize  []     = []
    hyphenize  (x:xs) = hyphenize' $ toLower x:xs
    hyphenize' []     = []
    hyphenize' (x:[]) = [toLower x]
    hyphenize' (x:xs) | isUpper x = '-':toLower x:hyphenize' xs
                      |  x == '.' = '-':hyphenize' xs
                      | otherwise = x:hyphenize' xs
    guessAuthor = Git.config "user.name"
    guessEmail = Git.config "user.email"
    getCurrentYear  = do
        (y,_,_) <- (toGregorian . utctDay) <$> getCurrentTime
        return $ show y
    guessTemplate = case CommandLineOption.template copt of
                      Just "hspec" -> BuiltInHSpec
                      Just "flat"  -> BuiltInFlat
                      _            -> fromMaybe BuiltInHSpec $ FromRepo <$> CommandLineOption.repository copt

defaultRepo :: String
defaultRepo = "git://github.com/fujimura/hi-hspec.git"
