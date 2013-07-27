module Main where

import qualified Distribution.Hi
import qualified Distribution.Hi.Version
import           Distribution.Hi.Option (getInitFlags, getMode)
import           Distribution.Hi.Types

main :: IO ()
main = do
    mode <- getMode
    case mode of
      ShowVersion -> print Distribution.Hi.Version.version
      Run         -> Distribution.Hi.cli =<< getInitFlags
