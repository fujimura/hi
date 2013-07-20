module Boilerplate where

import           Boilerplate.Compiler (compile)
import           Boilerplate.FilePath (toDestionationPath, modulePath)
import           Boilerplate.Context  (context)
import           Boilerplate.Option   (Options)
import           Boilerplate.Option   (InitFlags(..))
import           Boilerplate.Template (withTemplatesFromRepo)
import           Control.Arrow        ((&&&))
import           Control.Monad
import           Data.Map             ((!))
import           System.Directory     (createDirectoryIfMissing,
                                       getCurrentDirectory)
import           System.FilePath      (joinPath)

-- | Main function
cli :: InitFlags -> IO ()
cli initFlags = do
    currentDirecotory <- getCurrentDirectory

    withTemplatesFromRepo (repository initFlags) $ \templates -> do

      let sourceAndDestinations = map (id &&& toDestionationPath initFlags) templates

      createDirectoryIfMissing True $ joinPath [currentDirecotory, "src",  modulePath initFlags]
      createDirectoryIfMissing True $ joinPath [currentDirecotory, "test", modulePath initFlags]

      forM_ sourceAndDestinations $ \(s,d) -> compile s (joinPath [currentDirecotory, d]) $ context initFlags
