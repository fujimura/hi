{-# LANGUAGE OverloadedStrings #-}

module Distribution.Hi.Option
    (
      getInitFlags
    , getMode
    ) where

import           Control.Applicative
import           Data.Time.Calendar     (toGregorian)
import           Data.Time.Clock        (getCurrentTime, utctDay)
import           Distribution.Hi.Config (parseConfig)
import           Distribution.Hi.Flag   (extractInitFlags)
import           Distribution.Hi.Types
import           System.Console.GetOpt
import           System.Directory       (getHomeDirectory)
import           System.Environment     (getArgs)
import           System.FilePath        (joinPath)

-- | Available options.
options :: [OptDescr Arg]
options =
    [ Option ['p'] ["package-name"](ReqArg (Val "packageName") "package-name")  "Name of package"
    , Option ['m'] ["module-name"] (ReqArg (Val "moduleName") "Module.Name")  "Name of Module"
    , Option ['a'] ["author"]      (ReqArg (Val "author") "NAME")  "Name of the project's author"
    , Option ['e'] ["email"]       (ReqArg (Val "email") "EMAIL")  "Email address of the maintainer"
    , Option ['r'] ["repository"]  (ReqArg (Val "repository") "REPOSITORY")  "Template repository(optional)"
    , Option ['v'] ["version"]     (NoArg Version) "Show version number"
    , Option []    ["no-configuration-file"] (NoArg NoConfigurationFile) "Run without configuration file"
    , Option []    ["configuration-file"]    (ReqArg (Val "configFile") "CONFIGFILE") "Run with configuration file"
    ]

-- | Returns 'InitFlags'.
getInitFlags :: IO InitFlags
getInitFlags = do
    mode <- getMode
    case mode of
      Run                        -> runWithConfigurationFile
      RunWithNoConfigurationFile -> runWithNoConfigurationFile
      _                          -> error "Unexpected run mode"
  where
    runWithNoConfigurationFile = getInitFlagsPure =<< getArgs
    runWithConfigurationFile   = do
        (xs,_) <- parseArgs <$> getArgs
        ys     <- addYear =<< parseConfig =<< readFile =<< getConfigFileName
        return $ extractInitFlags (ys ++ xs)

getInitFlagsPure :: [String] -> IO InitFlags
getInitFlagsPure args = extractInitFlags <$> (addYear . fst . parseArgs $ args)

-- | Returns 'Mode'.
getMode :: IO Mode
getMode = do
    args <- fst <$> parseArgs <$> getArgs
    return $ if any id [True |Version <- args]
               then ShowVersion
               else if any id [True |NoConfigurationFile <- args]
                      then RunWithNoConfigurationFile
                      else Run

parseArgs :: [String] -> ([Arg], [String])
parseArgs argv =
   case getOpt Permute options argv of
      ([],_,errs) -> error $ concat errs ++ usageInfo header options
      (o,n,[]   ) -> (o,n)
      (_,_,errs ) -> error $ concat errs ++ usageInfo header options
  where
    header = "Usage: hi [OPTION...]"

defaultConfigFilePath :: IO FilePath
defaultConfigFilePath = do
    h <- getHomeDirectory
    return $ joinPath [h, defaultConfigFileName]

defaultConfigFileName :: FilePath
defaultConfigFileName = ".hirc"

getConfigFileName :: IO FilePath
getConfigFileName = do
    args <- (fst . parseArgs) <$> getArgs
    go args
  where
    go []                       = defaultConfigFilePath
    go ((Val "configFile" p):_) = return p
    go (_:xs)                   = go xs

addYear :: [Arg] -> IO [Arg]
addYear args = do
    (y,_,_) <- (toGregorian . utctDay) <$> getCurrentTime
    return $ (Val "year" $ show y):args
