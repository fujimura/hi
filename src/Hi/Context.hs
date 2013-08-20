{-# LANGUAGE RecordWildCards #-}
module Hi.Context
    (
      context
    ) where

import           Hi.Types
import qualified Data.Text          as T
import           Data.Text.Template (Context)

-- | Create a 'Context' from 'InitFlags Will raise error if the key was not found.
context :: InitFlags -> Context
context (InitFlags {..}) x = T.pack . lookup' $ T.unpack x
  -- TODO FIXME boilerplate
  where lookup' "packageName" = packageName
        lookup' "moduleName"  = moduleName
        lookup' "author"      = author
        lookup' "email"       = email
        lookup' "repository"  = repository
        lookup' "year"        = year
        lookup' k             = error $ "Key is not defined: " ++ k
