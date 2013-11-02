{-# LANGUAGE OverloadedStrings #-}

module Hi.Option
    (
      getInitFlags
    , getMode
    ) where

import           Hi.Config             (parseConfig)
import           Hi.Flag               (extractInitFlags)
import           Hi.Types

import           Control.Applicative
import           Data.Maybe            (fromMaybe)
import           Data.Time.Calendar    (toGregorian)
import           Data.Time.Clock       (getCurrentTime, utctDay)
import           System.Console.GetOpt
import           System.Directory      (doesFileExist, getHomeDirectory)
import           System.Environment    (getArgs)
import           System.FilePath       (joinPath)


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
    result <- go =<< getMode
    case result of
      Left  errors -> undefined
      Right x -> return $ x
  where
    go mode = case mode of
      Run                        -> runWithConfigurationFile
      RunWithNoConfigurationFile -> runWithNoConfigurationFile
      _                          -> error "Unexpected run mode"
    runWithNoConfigurationFile = getInitFlags' =<< getArgs
    runWithConfigurationFile   = do
        xs <- parseArgs <$> getArgs
        y  <- getCurrentYear
        ys <- do
            mfile <- readFileMaybe =<< getConfigFileName
            return $ fromMaybe [] (parseConfig <$> mfile)
        return $ extractInitFlags (xs ++ ys ++ [y])

-- | Return file contents in Maybe String or Nothing.
--
readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe f = do
    e <- doesFileExist f
    if e then Just <$> readFile f else return Nothing

-- | Returns `InitFlags` from given args, attaching year if it's missing in
-- args
getInitFlags' :: [String] -> IO (Either [Error] InitFlags)
getInitFlags' args = do
    y <- getCurrentYear
    return $ extractInitFlags $ (parseArgs $ args) ++ [y]

-- | Returns 'Mode'.
getMode :: IO Mode
getMode = do
    go . parseArgs <$> getArgs
  where
    go []                      = Run
    go (Version:_)             = ShowVersion
    go (NoConfigurationFile:_) = RunWithNoConfigurationFile
    go (_:xs)                  = go xs

parseArgs :: [String] -> [Arg]
parseArgs argv =
   case getOpt Permute options argv of
      ([],_,errs) -> error $ concat errs ++ usageInfo header options
      (o,_,[]   ) -> o
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
    go =<< parseArgs <$> getArgs
  where
    go []                       = defaultConfigFilePath
    go ((Val "configFile" p):_) = return p
    go (_:xs)                   = go xs

getCurrentYear :: IO Arg
getCurrentYear  = do
    (y,_,_) <- (toGregorian . utctDay) <$> getCurrentTime
    return $ (Val "year" $ show y)
