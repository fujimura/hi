module Main where

import qualified Boilerplate
import qualified Boilerplate.Version
import           Boilerplate.Option (getOptions, getMode, Mode(..))

main :: IO ()
main = do
    mode <- getMode
    case mode of
      ShowVersion -> print Boilerplate.Version.version
      Run         -> Boilerplate.cli =<< getOptions
