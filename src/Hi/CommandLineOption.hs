module Hi.CommandLineOption
  ( CommandLineOption(..)
  , commandLineOption
  , defaultRepo
  ) where

import           Options.Applicative

data CommandLineOption = CommandLineOption
                       { packageName             :: String
                       , moduleName              :: Maybe String
                       , author                  :: Maybe String
                       , email                   :: Maybe String
                       , repository              :: String
                       , configFilePath          :: Maybe String
                       , initializeGitRepository :: Maybe Bool
                       } deriving (Eq, Ord, Show)

commandLineOption :: Parser CommandLineOption
commandLineOption = CommandLineOption
   <$>          (strOption (short 'p' <> long "package-name" <> help "Name of package") <|> argument str (help "Name of package"))
   <*> optional (strOption (short 'm' <> long "moduleName"   <> help "Name of Module"))
   <*> optional (strOption (short 'a' <> long "author"       <> help "Name of the project's author"))
   <*> optional (strOption (short 'e' <> long "email"        <> help "Email address of the maintainer"))
   <*>           strOption (short 'r' <> long "repository"   <> help "Template repository" <> value defaultRepo)
   <*> optional (strOption (long "configuration-file"        <> help "Use specified configuration file"))
   <*> optional (switch    (long "initialize-git-repository" <> help "Initialize with git repository"))

defaultRepo :: String
defaultRepo = "git://github.com/fujimura/hi-hspec.git"
