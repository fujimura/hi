{-# LANGUAGE OverloadedStrings #-}

module Hi.Option
    (
      getOptions
    , getMode
    , usage
    ) where

import           Hi.Config             (parseConfig)
import           Hi.Types
import           Hi.Utils

import           Control.Applicative
import           Data.List             (intercalate)
import           Data.Maybe            (fromMaybe, mapMaybe)
import           Data.Time.Calendar    (toGregorian)
import           Data.Time.Clock       (getCurrentTime, utctDay)
import           System.Console.GetOpt
import           System.Directory      (doesFileExist, getHomeDirectory)
import qualified System.Environment
import           System.FilePath       (joinPath)


-- | Available options.
options :: [OptDescr Option]
options =
    [ Option ['p'] ["package-name"]       (ReqArg (Arg "packageName") "package-name") "Name of package"
    , Option ['m'] ["module-name"]        (ReqArg (Arg "moduleName" ) "Module.Name" ) "Name of Module"
    , Option ['a'] ["author"]             (ReqArg (Arg "author"     ) "NAME"        ) "Name of the project's author"
    , Option ['e'] ["email"]              (ReqArg (Arg "email"      ) "EMAIL"       ) "Email address of the maintainer"
    , Option ['r'] ["repository"]         (ReqArg (Arg "repository" ) "REPOSITORY"  ) "Template repository    ( optional ) "
    , Option []    ["configuration-file"] (ReqArg (Arg "configFile" ) "CONFIGFILE"  ) "Run with configuration file"
    , Option ['v'] ["version"]            (NoArg  Version)                            "Show version number"
    , Option ['h'] ["help"]               (NoArg  Help)                               "Display this help and exit"
    ]

-- | Returns 'Options'.
getOptions :: IO [Option]
getOptions = handleError
               <$> validateOptions
               =<< addDefaultRepo
               =<< addOptionsFromConfigFile
               =<< addYear
               =<< parseOptions
               <$> System.Environment.getArgs
  where
    addYear :: [Option] -> IO [Option]
    addYear vals = do
        y  <- getCurrentYear
        return $ vals ++ [y]

    addOptionsFromConfigFile :: [Option] -> IO [Option]
    addOptionsFromConfigFile vals = do
        repo <- do
            mfile <- readFileMaybe =<< getConfigFileName
            return $ fromMaybe [] (parseConfig <$> mfile)
        return $ vals ++ repo

    addDefaultRepo :: [Option] -> IO [Option]
    addDefaultRepo vals = return $ vals ++ [Arg "repository" defaultRepo]

    handleError :: Either [String] [Option] -> IO [Option]
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
    args <- parseOptions <$> System.Environment.getArgs
    return $ modeFor args
  where
    modeFor args | Help `elem` args    = ShowHelp
                 | Version `elem` args = ShowVersion
                 | otherwise           = Run

parseOptions :: [String] -> [Option]
parseOptions argv =
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
             defaultRepo ++ ".\n" ++
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
getConfigFileName = go =<< parseOptions <$> System.Environment.getArgs
  where
    go []                       = defaultConfigFilePath
    go ((Arg "configFile" p):_) = return p
    go (_:xs)                   = go xs

getCurrentYear :: IO Option
getCurrentYear  = do
    (y,_,_) <- (toGregorian . utctDay) <$> getCurrentTime
    return (Arg "year" $ show y)

-- | Validate given options
validateOptions :: [Option] -> Either [Error] [Option]
validateOptions values = case mapMaybe ($ values) validations of
                       []      -> Right values
                       errors  -> Left errors

validations ::[[Option] -> Maybe String]
validations = [ hasKey "packageName"
              , hasKey "moduleName"
              , hasKey "author"
              , hasKey "email"
              , hasKey "repository"
              , hasKey "year"
              ]

hasKey :: String -> [Option] -> Maybe String
hasKey k options = case lookupArg k options of
                      Just _  -> Nothing
                      Nothing -> Just $ "Could not find option: " ++ k
