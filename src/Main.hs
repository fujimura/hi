module Main where

import           Hi                    (run)
import           Hi.Option             (buildOption, defaultRepo)
import           Hi.Option.Commandline (commandLineOption)
import qualified Hi.Version            as Version

import           Options.Applicative

main :: IO ()
main = execParser opts >>= buildOption >>= run
  where
    opts = info (helper <*> (version <*> commandLineOption))
      ( fullDesc
     <> header "Generate a haskell project based on a template from github."
     <> footer footerText)
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
