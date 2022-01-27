module Hi.Cli
  ( run
  ) where

import qualified Hi
import           Hi.CommandLineOption         (CommandLineOption,
                                               commandLineOption, defaultRepo)
import           Hi.Option                    (buildOption)
import           Hi.Types

import           Data.Monoid                  (mempty, (<>))
import           Data.Version                 (showVersion)
import           Options.Applicative
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Paths_hi                     (version)

run :: [String] -> IO ()
run args = parseArgs args >>= Hi.run

parseArgs :: [String] -> IO Option
parseArgs args = handleParseResult (execParserPure (prefs showHelpOnEmpty) opts args) >>= buildOption

opts :: ParserInfo CommandLineOption
opts = info (helper <*> (version <*> commandLineOption))
  ( fullDesc
 <> header "Generate a haskell project based on a template from github."
 <> footerDoc (Just (PP.text footerText)))

footerText :: String
footerText = unlines [ ""
                     , "If repository is not provided, it defaults to the repository at"
                     , defaultRepo ++ "."
                     , ""
                     , "Example:"
                     , "    hi foo-bar"
                     ]

version :: Parser (a -> a)
version = infoOption (showVersion Paths_hi.version)
  (  short 'v'
  <> long "version"
  <> help "Print version information" )