module Hi.Cli
  ( run
  ) where

import qualified Hi
import           Hi.CommandLineOption         (CommandLineOption,
                                               commandLineOption)
import           Hi.Option                    (buildOption, defaultRepo)
import qualified Hi.Version                   as Version

import           Data.Monoid                  (mempty)
import           Options.Applicative
import qualified Text.PrettyPrint.ANSI.Leijen as PP

run :: [String] -> IO ()
run []   = showHelpText (prefs idm) opts
run args = (handleParseResult $ execParserPure (prefs idm) opts args) >>= buildOption >>= Hi.run

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
                     , "    hi --module-name 'Foo.Bar' --author 'you' --email 'you@gmail.com'"
                     ]

version :: Parser (a -> a)
version = infoOption Version.version
  (  short 'v'
  <> long "version"
  <> help "Print version information" )

showHelpText :: ParserPrefs -> ParserInfo a -> IO ()
showHelpText pprefs pinfo = handleParseResult . Failure $
  parserFailure pprefs pinfo ShowHelpText mempty
