{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Hi.Option
    (
      defaultRepo
    , buildOption
    ) where

import           Hi.CommandLineOption (CommandLineOption)
import qualified Hi.CommandLineOption as CommandLineOption
import           Hi.Config            (parseConfig)
import qualified Hi.Git               as Git
import           Hi.Types

import           Control.Applicative
import           Control.Monad
import           Data.Char            (isUpper, toLower)
import           Data.Maybe           (fromMaybe)
import           Data.Time.Calendar   (toGregorian)
import           Data.Time.Clock      (getCurrentTime, utctDay)

buildOption :: CommandLineOption -> IO Option
buildOption copt = do
    let packageName = (removeDup . hyphenize) cModuleName
    year <- getCurrentYear
    author <- guessAuthor
    email <- guessEmail
    template <- guessTemplate
    return $ Option { initializeGitRepository = fromMaybe False $ CommandLineOption.initializeGitRepository copt
                    , moduleName     = cModuleName
                    , packageName    = fromMaybe packageName $ CommandLineOption.packageName copt
                    , author         = author
                    , email          = email
                    , templateSource = template
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
    lookupConfig :: String -> IO (Maybe String)
    lookupConfig k = case CommandLineOption.configFilePath copt of
                       Just path -> (lookup k) . parseConfig <$> readFile path
                       Nothing   -> return Nothing
    choice :: [IO (Maybe String)] -> IO (Maybe String)
    choice xs = foldr1 mplus <$> sequence xs
    guessAuthor :: IO String
    guessAuthor = do
      mc <- choice [ (return $ CommandLineOption.author copt)
                   , (lookupConfig "author")
                   , (Git.config "user.name")
                   ]
      case mc of
        Just x -> return x
        Nothing -> fail "No user specified"
    guessEmail :: IO String
    guessEmail  = do
      mc <- choice [ (return $ CommandLineOption.email copt)
                   , (lookupConfig "email")
                   , (Git.config "user.email")
                   ]
      case mc of
        Just x -> return x
        Nothing -> fail "No email specified"
    getCurrentYear :: IO String
    getCurrentYear  = do
        (y,_,_) <- (toGregorian . utctDay) <$> getCurrentTime
        return $ show y
    guessTemplate :: IO TemplateSource
    guessTemplate = return . FromRepo $ maybe defaultRepo id (CommandLineOption.repository copt)

defaultRepo :: String
defaultRepo = "git://github.com/fujimura/hi-hspec.git"
