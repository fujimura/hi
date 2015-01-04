{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Hi.Option
    (
      buildOption
    ) where

import           Hi.CommandLineOption (CommandLineOption)
import qualified Hi.CommandLineOption as CommandLineOption
import           Hi.Config            (parseConfig)
import qualified Hi.Git               as Git
import           Hi.Types

import           Control.Applicative
import           Control.Monad
import           Data.Char            (toUpper)
import           Data.Maybe           (catMaybes, fromMaybe)
import           Data.Time.Calendar   (toGregorian)
import           Data.Time.Clock      (getCurrentTime, utctDay)

buildOption :: CommandLineOption -> IO Option
buildOption copt = do
    let moduleName = modularize $ CommandLineOption.packageName copt
    year <- getCurrentYear
    author <- guessAuthor
    email <- guessEmail
    let mGitInit = if CommandLineOption.initializeGitRepository copt
                     then Just "git init && git add . && git commit -m \"Initial commit\""
                     else Nothing
        afterCommands = catMaybes [ mGitInit
                                  , CommandLineOption.afterCommand copt
                                  ]
    return Option { moduleName     = fromMaybe moduleName $ CommandLineOption.moduleName copt
                  , packageName    = CommandLineOption.packageName copt
                  , author         = author
                  , email          = email
                  , templateSource = FromRepo $ CommandLineOption.repository copt
                  , year           = year
                  , afterCommands  = afterCommands
                  }
  where
    lookupConfig :: String -> IO (Maybe String)
    lookupConfig k = case CommandLineOption.configFilePath copt of
                       Just path -> lookup k . parseConfig <$> readFile path
                       Nothing   -> return Nothing
    choice :: [IO (Maybe String)] -> IO (Maybe String)
    choice xs = foldr1 mplus <$> sequence xs
    guessAuthor :: IO String
    guessAuthor = do
      mc <- choice [ return $ CommandLineOption.author copt
                   , lookupConfig "author"
                   , Git.config "user.name"
                   ]
      case mc of
        Just x -> return x
        Nothing -> fail "No user specified"
    guessEmail :: IO String
    guessEmail  = do
      mc <- choice [ return $ CommandLineOption.email copt
                   , lookupConfig "email"
                   , Git.config "user.email"
                   ]
      case mc of
        Just x -> return x
        Nothing -> fail "No email specified"
    getCurrentYear :: IO String
    getCurrentYear  = do
        (y,_,_) <- (toGregorian . utctDay) <$> getCurrentTime
        return $ show y

-- | Capitalize words and connect them with periods
--
-- >>> modularize "package"
-- "Package"
--
-- >>> modularize "package-name"
-- "Package.Name"
--
-- >>> modularize "another-package-name"
-- "Another.Package.Name"
--
modularize :: String -> String
modularize []     = []
modularize [x]    = [toUpper x]
modularize (x:xs) = toUpper x : rest xs
  where
    rest []       = []
    rest ('-':ys) = '.' : modularize ys
    rest (y:ys)   = y:rest ys
