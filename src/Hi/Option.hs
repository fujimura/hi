{-# LANGUAGE OverloadedStrings #-}

module Hi.Option
    (
      getOptions
    , getMode
    , options
    , usage
    ) where

import           Hi.Config             (parseConfig)
import           Hi.Types
import           Hi.Utils

import           Control.Applicative
import           Data.Char             (isUpper, toLower)
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
    [ Option ['m'] ["module-name"]        (ReqArg (Arg "moduleName" ) "Module.Name" ) "Name of Module"
    , Option ['p'] ["package-name"]       (ReqArg (Arg "packageName") "package-name") "Name of package        ( optional )"
    , Option ['a'] ["author"]             (ReqArg (Arg "author"     ) "NAME"        ) "Name of the project's author"
    , Option ['e'] ["email"]              (ReqArg (Arg "email"      ) "EMAIL"       ) "Email address of the maintainer"
    , Option ['r'] ["repository"]         (ReqArg (Arg "repository" ) "REPOSITORY"  ) "Template repository    ( optional )"
    , Option []    ["configuration-file"] (ReqArg (Arg "configFile" ) "CONFIGFILE"  ) "Run with configuration file"
    , Option ['v'] ["version"]            (NoArg  Version)                            "Show version number"
    , Option []    ["initialize-git-repository"] (NoArg InitializeGitRepository)      "Initialize with git repository"
    , Option ['h'] ["help"]               (NoArg  Help)                               "Display this help and exit"
    ]

toOption :: (String, String) -> Maybe Option
toOption (key, value) = maybe err ok $ key `lookupOption` options
  where
    err = error $ "Invalid options \"" ++ key ++ "\" was specified"

    ok :: OptDescr Option -> Maybe Option
    ok (Option _ _ argDescr _) = toOption' argDescr value

    lookupOption :: String -> [OptDescr Option] -> Maybe (OptDescr Option)
    lookupOption k opts = k `lookup` map (\x@(Option _ (longOpt:_) _ _) -> (longOpt,x)) opts

    toOption' :: ArgDescr Option -> String -> Maybe Option
    toOption' (NoArg opt) "True" = Just opt
    toOption' (NoArg _) _        = Nothing
    toOption' (ReqArg f _) val   = Just $ f val
    toOption' (OptArg _ _) _     = err

-- | Returns 'Options'.
getOptions :: IO [Option]
getOptions = handleError
               <$> validateOptions
               =<< addDefaultRepo
               =<< addPackageNameIfMissing
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
            return $ mapMaybe toOption $ fromMaybe [] (parseConfig <$> mfile)
        return $ vals ++ repo

    addDefaultRepo :: [Option] -> IO [Option]
    addDefaultRepo vals = return $ vals ++ [Arg "repository" defaultRepo]

    addPackageNameIfMissing :: [Option] -> IO [Option]
    addPackageNameIfMissing vals =
        return $ case ("packageName" `lookupArg` vals, "moduleName" `lookupArg` vals) of
          (Nothing, Just m)  -> vals ++ [Arg "packageName" $ (removeDup . hyphenize) m]
          _                  -> vals
      where
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
             "    hi --module-name 'Foo.Bar' " ++
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
hasKey k opts = case lookupArg k opts of
                      Just _  -> Nothing
                      Nothing -> Just $ "Could not find option: " ++ k
