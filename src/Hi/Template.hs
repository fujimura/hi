module Hi.Template
    (
      readTemplates
    , untemplate
    ) where

import           Hi.Directory        (inTemporaryDirectory)
import qualified Hi.Git              as Git
import           Hi.Types

import           Data.List.Split     (splitOn)

-- | Read templates in given 'FilePath'
readTemplates :: FilePath -> IO Files
readTemplates repo =
    inTemporaryDirectory "hi" $ do
        -- TODO Handle error
        _ <- Git.clone $ Git.expandUrl repo
        paths <- Git.lsFiles
        contents <- mapM readFile paths
        return $ zip paths contents

-- | Remove \".template\" from 'FilePath'
untemplate :: FilePath -> FilePath
untemplate = head . splitOn ".template"
