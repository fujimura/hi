module Main where

import qualified Boilerplate
import qualified Boilerplate.Version
import           Boilerplate.Option (getInitFlags, getMode)
import           Boilerplate.Types

main :: IO ()
main = do
    mode <- getMode
    case mode of
      ShowVersion -> print Boilerplate.Version.version
      Run         -> Boilerplate.cli =<< getInitFlags
