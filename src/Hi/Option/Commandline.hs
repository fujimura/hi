module Hi.Option.Commandline
  ( CommandLineOption(..)
  , commandLineOption
  ) where

import           Options.Applicative

data CommandLineOption = CommandLineOption
                       { moduleName              :: String
                       , packageName             :: Maybe String
                       , author                  :: Maybe String
                       , email                   :: Maybe String
                       , repository              :: Maybe String
                       , initializeGitRepository :: Maybe Bool
                       } deriving (Eq, Ord, Show)

commandLineOption :: Parser CommandLineOption
commandLineOption = CommandLineOption
   <$> strOption ( short 'm' <> long "moduleName" <> metavar "Module.Name" <> help "Name of Module" )
   <*> optional ( strOption ( short 'p' <> long "packageName" <> metavar "package-name" <> help "Name of package" ))
   <*> optional ( strOption ( short 'a' <> long "author"      <> metavar "NAME"         <> help "Name of the project's author" ))
   <*> optional ( strOption ( short 'e' <> long "email"       <> metavar "EMAIL"        <> help "Email address of the maintainer" ))
   <*> optional ( strOption ( short 'r' <> long "repository"  <> metavar "REPOSITORY"   <> help "Template repository" ))
   <*> optional ( switch    ( long "initialize-git-repository" <> help "Initialize with git repository"))
