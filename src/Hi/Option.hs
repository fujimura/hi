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
import           Data.List             (intercalate)
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
    [ Option ['p'] ["package-name"]       (ReqArg (Val "packageName") "package-name") "Name of package"
    , Option ['m'] ["module-name"]        (ReqArg (Val "moduleName" ) "Module.Name" ) "Name of Module"
    , Option ['a'] ["author"]             (ReqArg (Val "author"     ) "NAME"        ) "Name of the project's author"
    , Option ['e'] ["email"]              (ReqArg (Val "email"      ) "EMAIL"       ) "Email address of the maintainer"
    , Option ['r'] ["repository"]         (ReqArg (Val "repository" ) "REPOSITORY"  ) "Template repository    ( optional ) "
    , Option []    ["configuration-file"] (ReqArg (Val "configFile" ) "CONFIGFILE"  ) "Run with configuration file"
    , Option ['v'] ["version"]            (NoArg  Version)                            "Show version number"
    ]

-- | Returns 'InitFlags'.
getInitFlags :: IO InitFlags
getInitFlags = handleError
               <$> extractInitFlags
               =<< addDefaultRepo
               =<< addArgsFromConfigFile
               =<< addYear
               =<< parseArgs
               <$> getArgs
  where
    addYear :: [Arg] -> IO [Arg]
    addYear vals = do
        y  <- getCurrentYear
        return $ vals ++ [y]

    addArgsFromConfigFile :: [Arg] -> IO [Arg]
    addArgsFromConfigFile vals = do
        repo <- do
            mfile <- readFileMaybe =<< getConfigFileName
            return $ fromMaybe [] (parseConfig <$> mfile)
        return $ vals ++ repo

    addDefaultRepo :: [Arg] -> IO [Arg]
    addDefaultRepo vals = return $ vals ++ [Val "repository" defaultRepo]

    handleError :: Either [String] InitFlags -> IO InitFlags
    handleError result = case result of
        Left  errors -> error $ (intercalate "\n" errors) ++ "\n (Run with no arguments to see usage)"
        Right x      -> return x

-- | Return file contents in Maybe String or Nothing.
--
readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe f = do
    e <- doesFileExist f
    if e then Just <$> readFile f else return Nothing

-- | Returns 'Mode'.
getMode :: IO Mode
getMode = do
    args <- parseArgs <$> getArgs
    return $ if Version `elem` args then ShowVersion else Run

parseArgs :: [String] -> [Arg]
parseArgs argv =
  case getOpt Permute options argv of
    ([],_,errs) -> error $ concat errs ++ usage
    (o,_,[]   ) -> o
    (_,_,errs ) -> error $ concat errs ++ usage

usage :: String
usage = usageInfo header options ++ footer
  where
    header = "Usage: hi [OPTION...]\n" ++
             "Generate a haskell project based on a template from github.\n"
    footer = "\n" ++
             "If repository is not provided, it defaults to the repository at\n" ++
             "https://github.com/fujimura/hi-hspec.\n" ++
             "\n" ++
             "Example:\n" ++
             "    hi --package-name 'foo-bar' --module-name 'Foo.Bar' " ++
             "--author 'you' --email 'you@gmail.com'"

defaultConfigFilePath :: IO FilePath
defaultConfigFilePath = do
    h <- getHomeDirectory
    return $ joinPath [h, defaultConfigFileName]

defaultConfigFileName :: FilePath
defaultConfigFileName = ".hirc"

defaultRepo :: String
defaultRepo = "git://github.com/fujimura/hi-hspec.git"

getConfigFileName :: IO FilePath
getConfigFileName = go =<< parseArgs <$> getArgs
  where
    go []                       = defaultConfigFilePath
    go ((Val "configFile" p):_) = return p
    go (_:xs)                   = go xs

getCurrentYear :: IO Arg
getCurrentYear  = do
    (y,_,_) <- (toGregorian . utctDay) <$> getCurrentTime
    return (Val "year" $ show y)
