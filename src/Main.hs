module Main where

import           Hi                           (run)
import           Hi.Option                    (buildOption, defaultRepo)
import           Hi.Option.Commandline        (commandLineOption)
import qualified Hi.Version                   as Version

import           Data.Monoid                  (mempty)
import           Options.Applicative
import           System.Environment           (getArgs)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

main :: IO ()
main = do
    args <- getArgs
    if null args
      then showHelpText (prefs idm) opts
      else execParser opts >>= buildOption >>= run
  where
    opts = info (helper <*> (version <*> commandLineOption))
      ( fullDesc
     <> header "Generate a haskell project based on a template from github."
     <> footerDoc (Just (PP.text footerText)))
    -- TODO Newline won't rendered.
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
