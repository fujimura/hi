{-# LANGUAGE OverloadedStrings #-}

module Distribution.Hi.Option
    (
      getInitFlags
    , getMode
    ) where

import           Control.Applicative
import           Control.Exception      (IOException, catch)
import           Data.Time.Calendar     (toGregorian)
import           Data.Time.Clock        (getCurrentTime, utctDay)
import           Distribution.Hi.Config (parseConfig)
import           Distribution.Hi.Flag   (extractInitFlags)
import           Distribution.Hi.Types
import           Prelude                hiding (catch)
import           System.Console.GetOpt
import           System.Directory       (getHomeDirectory)
import           System.Environment     (getArgs)
import           System.FilePath        (joinPath)
import           System.IO              (hPutStr, stderr)


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
    runWithNoConfigurationFile = getInitFlags' =<< getArgs
    runWithConfigurationFile   = do
        (xs,_) <- parseArgs <$> getArgs
        ys     <- addYear <$> parseConfig =<< readFile' =<< getConfigFileName
        return $ extractInitFlags (ys ++ xs)
    readFile' f = catch (readFile f)
                  (\e -> do let err = show (e :: IOException)
                            hPutStr stderr ("Warning: Couldn't open " ++ f ++ ": " ++ err)
                            return "")

-- | Returns `InitFlags` from given args, attaching year if it's missing in
-- args
getInitFlags' :: [String] -> IO InitFlags
getInitFlags' args = extractInitFlags <$> (addYear . fst . parseArgs $ args)

-- | Returns 'Mode'.
getMode :: IO Mode
getMode = do
    go . fst . parseArgs <$> getArgs
  where
    go []                      = Run
    go (Version:_)             = ShowVersion
    go (NoConfigurationFile:_) = RunWithNoConfigurationFile
    go (_:xs)                  = go xs

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
    go =<< (fst . parseArgs) <$> getArgs
  where
    go []                       = defaultConfigFilePath
    go ((Val "configFile" p):_) = return p
    go (_:xs)                   = go xs

addYear :: [Arg] -> IO [Arg]
addYear args = do
    (y,_,_) <- (toGregorian . utctDay) <$> getCurrentTime
    return $ (Val "year" $ show y):args
