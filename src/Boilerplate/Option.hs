{-# LANGUAGE OverloadedStrings #-}

module Boilerplate.Option
    (
      getInitFlags
    , getMode
    , Options
    , InitFlags(..)
    , Mode(..)
    ) where

import           Control.Applicative
import qualified Data.Map              as Map
import           Data.Time.Calendar    (toGregorian)
import           Data.Time.Clock       (getCurrentTime, utctDay)
import           System.Console.GetOpt
import           System.Environment    (getArgs)

type Options = Map.Map String String

type Label = String

-- | Arguments.
data Arg = Version | Name String | Val Label String deriving(Eq, Show)

-- | Run mode.
data Mode = ShowVersion | Run deriving(Eq, Show)

type Flag = String

data InitFlags =
    InitFlags { packageName :: Flag
              , moduleName  :: Flag
              , author      :: Flag
              , email       :: Flag
              , repository  :: Flag
              , year        :: Flag
              }

-- | Available options.
options :: [OptDescr Arg]
options =
    [ Option ['p'] ["package-name"](ReqArg (Val "packageName") "package-name")  "Name of package"
    , Option ['m'] ["module-name"] (ReqArg (Val "moduleName") "Module.Name")  "Name of Module"
    , Option ['a'] ["author"]      (ReqArg (Val "author") "NAME")  "Name of the project's author"
    , Option ['e'] ["email"]       (ReqArg (Val "email") "EMAIL")  "Email address of the maintainer"
    , Option ['r'] ["repository"]  (ReqArg (Val "repository") "REPOSITORY")  "Template repository(optional)"
    , Option ['v'] ["version"]     (NoArg Version) "show version number"
    ]

getOptions :: IO Options
getOptions = do
    addYear =<< addRepo . extractOptions <$> fst <$> parseArgs <$> getArgs

getInitFlags :: IO InitFlags
getInitFlags = do
    flags' <- addYear' =<< fst <$> parseArgs <$> getArgs
    return $ extractInitFlags flags'

getMode :: IO Mode
getMode = do
    args <- fst <$> parseArgs <$> getArgs
    return $ if any id [True |Version <- args]
               then ShowVersion
               else Run

parseArgs :: [String] -> ([Arg], [String])
parseArgs argv =
   case getOpt Permute options argv of
      ([],_,errs) -> error $ concat errs ++ usageInfo header options
      (o,n,[]   ) -> (o,n)
      (_,_,errs ) -> error $ concat errs ++ usageInfo header options
  where
    header = "Usage: boilerplate [OPTION...]"

extractOptions :: [Arg] -> Options
extractOptions args = Map.fromList $ [(l,v) | (Val l v) <- args]

extractInitFlags :: [Arg] -> InitFlags
extractInitFlags args = InitFlags { packageName = lookupPackageName args
                                  , moduleName  = lookupModuleName args
                                  , author      = lookupAuthor args
                                  , email       = lookupEmail args
                                  , repository  = lookupRepository args
                                  , year        = lookupRepository args
                                  }
  where
    lookupPackageName = lookup' "packageName"
    lookupModuleName  = lookup' "moduleName"
    lookupAuthor      = lookup' "author"
    lookupEmail       = lookup' "email"
    lookupRepository  = lookup' "repository"
    lookup' label args = case lookup label $ [(l, v) | (Val l v) <- args] of
                            Just v  -> v
                            Nothing -> if label == "repository"
                                         then defaultRepo
                                         else error $ "Cound not find key :" ++ label

addYear' :: [Arg] -> IO [Arg]
addYear' args = do
    (y,_,_) <- (toGregorian . utctDay) <$> getCurrentTime
    return $ (Val "year" $ show y):args

addYear :: Options -> IO Options
addYear opts = do
    (y,_,_) <- (toGregorian . utctDay) <$> getCurrentTime
    return $ Map.insert "year" (show y) opts

defaultRepo :: String
defaultRepo = "git://github.com/fujimura/boilerplate-hspec.git"

addRepo :: Options -> Options
addRepo opts = if Map.member "repository" opts
                 then opts
                 else Map.insert "repository" defaultRepo opts
