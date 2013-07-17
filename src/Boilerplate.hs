module Boilerplate where

import           Boilerplate.Compiler (compile)
import           Boilerplate.FilePath (toDestionationPath, modulePath)
import           Boilerplate.Context  (context)
import           Boilerplate.Option   (Options)
import           Boilerplate.Template (withTemplatesFromRepo)
import           Control.Arrow        ((&&&))
import           Control.Monad
import           Data.Map             ((!))
import           System.Directory     (createDirectoryIfMissing,
                                       getCurrentDirectory)
import           System.FilePath      (joinPath)

cli :: Options -> IO ()
cli options = do
    currentDirecotory <- getCurrentDirectory

    withTemplatesFromRepo (options ! "repository") $ \templates -> do

      let sourceAndDestinations = map (id &&& toDestionationPath options) templates

      createDirectoryIfMissing True $ joinPath [currentDirecotory, "src",  modulePath options]
      createDirectoryIfMissing True $ joinPath [currentDirecotory, "test", modulePath options]

      forM_ sourceAndDestinations $ \(s,d) -> compile s (joinPath [currentDirecotory, d]) $ context options
