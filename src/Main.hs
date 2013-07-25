module Main where

import qualified Hi
import qualified Hi.Version
import           Hi.Option (getInitFlags, getMode)
import           Hi.Types

main :: IO ()
main = do
    mode <- getMode
    case mode of
      ShowVersion -> print Hi.Version.version
      Run         -> Hi.cli =<< getInitFlags
