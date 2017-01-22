{-# LANGUAGE PackageImports #-}
module Hi.Directory
    (
      inTemporaryDirectory
    , inDirectory
    ) where

import           Control.Exception    (bracket_)
import           System.Directory
import "temporary" System.IO.Temp       (withSystemTempDirectory)

-- |Run callback in a temporary directory.
inTemporaryDirectory :: String         -- ^ Base of temporary directory name
                     -> IO a -> IO a -- ^ Callback
inTemporaryDirectory name callback =
    withSystemTempDirectory name $ flip inDirectory callback


-- |Run callback in given directory.
inDirectory :: FilePath        -- ^ Filepath to run callback
            -> IO a -> IO a  -- ^ Callback
inDirectory path callback = do
    pwd <- getCurrentDirectory
    bracket_ (setCurrentDirectory path) (setCurrentDirectory pwd) callback
