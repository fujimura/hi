module Hi.Template
    (
      isTemplate
    , readTemplates
    , untemplate
    ) where

import           Control.Exception   (bracket_)
import           Hi.Directory        (inTemporaryDirectory)
import qualified Hi.Git              as Git
import           Hi.Types

import           Control.Applicative ((<$>))

import qualified Data.ByteString     as BS (readFile)
import           Data.List           (isSuffixOf)
import           Data.List.Split     (splitOn)

import           Paths_hi            (getDataDir)
import           System.Directory
import           System.FilePath

-- | Read templates in given 'FilePath'
readTemplates :: TemplateSource -> IO Files
readTemplates BuiltInHSpec = readBuiltInTemplate "hspec"
readTemplates BuiltInFlat  = readBuiltInTemplate "flat"
readTemplates (FromRepo repo) =
    inTemporaryDirectory "hi" $ do
        -- TODO Handle error
        _ <- Git.clone $ Git.expandUrl repo
        paths <- Git.lsFiles
        mapM fetchFile paths

readBuiltInTemplate :: String -> IO Files
readBuiltInTemplate dir = do
    root <- getDataDir
    inDirectory (root </> "templates" </> dir) $ do
      paths <- getDirectoryContentsRecursively "./"
      mapM fetchFile paths

inDirectory :: FilePath -> IO a -> IO a
inDirectory path action = do
    pwd <- getCurrentDirectory
    let go    = setCurrentDirectory path
        back  = setCurrentDirectory pwd
    bracket_ go back action

getDirectoryContentsRecursively :: FilePath -> IO [FilePath]
getDirectoryContentsRecursively path = go [path]
  where
    go :: [FilePath] -> IO [FilePath]
    go [] = return []
    go (x:xs) = do
       isDir <- doesDirectoryExist x
       if isDir then do
                  xs' <- filter (/= "..") <$> filter (/= ".") <$> getDirectoryContents x
                  go (xs ++ map (x </>) xs')
                else
                  (x:) <$> go xs

fetchFile :: FilePath -> IO File
fetchFile fp | isTemplate fp = TemplateFile fp <$> BS.readFile fp
             | otherwise     = RegularFile  fp <$> BS.readFile fp

-- | Determine if a given filepath is a template file based on its extension
-- >>> isTemplate "Example.hs.template"
-- True
-- >>> isTemplate "NotATemplate.hs"
-- False
isTemplate :: FilePath -> Bool
isTemplate = isSuffixOf ".template"

-- | Remove \".template\" from 'FilePath'
untemplate :: FilePath -> FilePath
untemplate = head . splitOn ".template"
