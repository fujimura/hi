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

    putStrLn $ "Creating new project from repository: " ++ repository

    withTemplatesFromRepo repository $ \templates -> do

      let sourceAndDestinations = map (id &&& toDestionationPath initFlags) templates

      forM_ sourceAndDestinations $ \(src,dst) -> do
        let dst' = joinPath [currentDirecotory, dst]
        createDirectoryIfMissing True $ dropFileName dst'
        putStrLn $ "    " ++ green "create" ++ "  " ++ dst
        compile src dst' $ context initFlags

green :: String -> String
green x = "\x1b[32m" ++ x ++ "\x1b[0m"
