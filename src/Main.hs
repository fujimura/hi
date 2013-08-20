module Main where

import qualified Hi         as Hi
import           Hi.Option  (getInitFlags, getMode)
import           Hi.Types
import           Hi.Version (version)

main :: IO ()
main = do
    mode <- getMode
    case mode of
      ShowVersion                -> putStrLn version
      RunWithNoConfigurationFile -> Hi.cli =<< getInitFlags
      Run                        -> Hi.cli =<< getInitFlags
