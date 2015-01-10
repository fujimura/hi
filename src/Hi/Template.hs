module Hi.Template
    (
      isTemplate
    , readTemplates
    , untemplate
    ) where

import           Hi.Directory        (inTemporaryDirectory)
import qualified Hi.Git              as Git
import           Hi.Types

import           Control.Applicative ((<$>))

import qualified Data.ByteString     as BS (readFile)
import           Data.List           (isSuffixOf)
import           Data.List.Split     (splitOn)
import           System.Directory    (canonicalizePath, doesDirectoryExist)

-- | Read templates in given 'TemplateSource'
readTemplates :: TemplateSource -> IO Files
readTemplates (FromRepo repo) = do
    e <- doesDirectoryExist repo
    repo' <- if e
               -- It seems to be an file path
               then canonicalizePath repo
               -- Not looks like a file path
               else return repo
    inTemporaryDirectory "hi" $ do
        Git.clone $ Git.expandUrl repo'
        paths <- Git.lsFiles
        mapM fetchFile paths

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
