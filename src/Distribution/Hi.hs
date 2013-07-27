{-# LANGUAGE NamedFieldPuns #-}
module Distribution.Hi where

import           Distribution.Hi.Compiler (compile)
import           Distribution.Hi.FilePath (toDestionationPath, toDir)
import           Distribution.Hi.Context  (context)
import           Distribution.Hi.Types
import           Distribution.Hi.Template (withTemplatesFromRepo)
import           Control.Arrow        ((&&&))
import           Control.Monad
import           System.Directory     (createDirectoryIfMissing,
                                       getCurrentDirectory)
import           System.FilePath      (joinPath)

-- | Main function
cli :: InitFlags -> IO ()
cli initFlags@(InitFlags {moduleName, repository}) = do
    currentDirecotory <- getCurrentDirectory

    withTemplatesFromRepo (repository) $ \templates -> do

      let sourceAndDestinations = map (id &&& toDestionationPath initFlags) templates

      createDirectoryIfMissing True $ joinPath [currentDirecotory, "src",  toDir moduleName]
      createDirectoryIfMissing True $ joinPath [currentDirecotory, "test", toDir moduleName]

      forM_ sourceAndDestinations $ \(s,d) -> compile s (joinPath [currentDirecotory, d]) $ context initFlags
