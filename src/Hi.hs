{-# LANGUAGE NamedFieldPuns #-}
module Hi where

import           Hi.Compiler (compile)
import           Hi.FilePath (toDestionationPath)
import           Hi.Context  (context)
import           Hi.Types
import           Hi.Template (withTemplatesFromRepo)
import           Control.Arrow        ((&&&))
import           Control.Monad
import           System.Directory     (createDirectoryIfMissing,
                                       getCurrentDirectory)
import           System.FilePath      (joinPath, dropFileName)

-- | Main function
cli :: InitFlags -> IO ()
cli initFlags@(InitFlags {repository}) = do
    currentDirecotory <- getCurrentDirectory

    withTemplatesFromRepo repository $ \templates -> do

      let sourceAndDestinations = map (id &&& toDestionationPath initFlags) templates

      forM_ sourceAndDestinations $ \(src,dst) -> do
        let dst' = joinPath [currentDirecotory, dst]
        createDirectoryIfMissing True $ dropFileName dst'
        compile src dst' $ context initFlags
