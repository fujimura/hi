module Main where

import qualified Distribution.Hi         as Hi
import           Distribution.Hi.Option  (getInitFlags, getMode)
import           Distribution.Hi.Types
import           Distribution.Hi.Version (version)

main :: IO ()
main = do
    mode <- getMode
    case mode of
      ShowVersion                -> putStrLn version
      RunWithNoConfigurationFile -> Hi.cli =<< getInitFlags
      Run                        -> Hi.cli =<< getInitFlags
