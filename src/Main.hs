module Main where

import           Hi         (run)
import           Hi.Option  (getOptions, getMode, usage)
import           Hi.Types
import           Hi.Version (version)

main :: IO ()
main = do
    mode <- getMode
    case mode of
      ShowHelp -> putStrLn usage
      ShowVersion -> putStrLn version
      _           -> run =<< getOptions
