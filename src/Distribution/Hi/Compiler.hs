module Distribution.Hi.Compiler
    (
      compile
    ) where

import qualified Data.Text.IO       as T
import qualified Data.Text.Lazy.IO  as LT
import           Data.Text.Template (Context, substitute)

-- | Compile a file from template with given 'Context'.
compile :: FilePath -- Source
        -> FilePath -- Destionation
        -> Context  -- Context
        -> IO ()
compile src dst ctx = do
    tmpl <- T.readFile src
    LT.writeFile dst $ substitute tmpl ctx
