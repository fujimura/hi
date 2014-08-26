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

-- | Read templates in given 'FilePath'
readTemplates :: FilePath -> IO Files
readTemplates repo =
    inTemporaryDirectory "hi" $ do
        -- TODO Handle error
        _ <- Git.clone $ Git.expandUrl repo
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
