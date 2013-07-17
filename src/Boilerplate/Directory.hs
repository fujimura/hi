module Boilerplate.Directory
    (
      inDirectory
    , inTemporaryDirectory
    ) where

import           Control.Exception    (bracket_)
import           System.Directory
import           System.IO.Temp       (withSystemTempDirectory)

inTemporaryDirectory :: String -> IO a -> IO a
inTemporaryDirectory name cb =
    withSystemTempDirectory name $ flip inDirectory cb

inDirectory :: FilePath -> IO a -> IO a
inDirectory path cb = do
    pwd <- getCurrentDirectory
    bracket_ (setCurrentDirectory path) (setCurrentDirectory pwd) cb
