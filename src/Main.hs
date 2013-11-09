module Main where

import           Hi         (run)
import           Hi.Option  (getInitFlags, getMode)
import           Hi.Types
import           Hi.Version (version)

main :: IO ()
main = do
    mode <- getMode
    case mode of
      ShowVersion -> putStrLn version
      _           -> run =<< getInitFlags mode
