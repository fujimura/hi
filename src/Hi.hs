module Hi
  (
    run
  , process
  ) where

import           Hi.Directory             (inDirectory)
import           Hi.FilePath              (rewritePath)
import qualified Hi.Git                   as Git
import           Hi.Template              (readTemplates)
import           Hi.Types
import           Hi.Utils

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString          as BS (writeFile, concat)
import qualified Data.ByteString.Lazy     as LBS (toChunks)
import           Data.Maybe               (fromJust)
import qualified Data.Text                as T (pack, unpack)
import           Data.Text.Encoding       (decodeUtf8)
import           Data.Text.Lazy.Encoding  (encodeUtf8)
import           Data.Text.Template       (Context, substitute)
import           System.Directory         (createDirectoryIfMissing)
import           System.FilePath          (dropFileName)
import           System.Process           (system)

-- | Run 'hi'.
run :: [Option] -> IO ()
run options = do
    putStrLn $ "Creating new project from repository: " ++ Git.expandUrl repository
    writeFiles =<< showFileList =<< process options <$> readTemplates repository
    postProcess options
  where
    repository = fromJust $ lookupArg "repository" options

-- |Write given 'Files' to filesystem.
writeFiles :: Files -> IO ()
writeFiles = mapM_ write

write :: File -> IO ()
write f = let path = getFilePath f
              contents = getFileContents f
          in createDirectoryIfMissing True (dropFileName path) >>
               BS.writeFile path contents

-- | Show 'Files' to stdout.
showFileList :: Files -> IO Files
showFileList files = do
    mapM_ (showFile . getFilePath) files
    return files
  where
    showFile :: FilePath -> IO ()
    showFile path = putStrLn $ "    " ++ green "create" ++ "  " ++ path
    green :: String -> String
    green x = "\x1b[32m" ++ x ++ "\x1b[0m"

-- |Process given 'Files' and return result. it does
--
-- 1. rewrite path
--
-- 2. substitute arguments
process :: [Option] -> Files -> Files
process options = map go
  where
    go (TemplateFile path content) = TemplateFile (rewritePath' path) (substitute' content)
    go (RegularFile  path content) = RegularFile  (rewritePath' path) content
    rewritePath'     = rewritePath options
    substitute' text = BS.concat . LBS.toChunks . encodeUtf8 $
                        substitute (decodeUtf8 text) (context options)

-- | Return 'Context' obtained by given 'Options'
context :: [Option] -> Context
context options x = T.pack (fromJust $ lookup (T.unpack x) [(k,v) | (Arg k v) <- options])

postProcess :: [Option] -> IO ()
postProcess options = do
    when (InitializeGitRepository `elem` options) $
      -- TODO This wont' work unless template has `package-name` as root dir.
      inDirectory (fromJust $ lookupArg "packageName" options) $
        void $ system "git init && git add . && git commit -m \"Initial commit\""
    return ()
