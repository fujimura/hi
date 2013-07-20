{-# LANGUAGE RecordWildCards #-}
module Boilerplate.Context
    (
      context
    ) where

import           Boilerplate.Option (Options)
import           Boilerplate.Option (InitFlags(..))
import           Control.Arrow      ((***))
import qualified Data.Map           as Map
import qualified Data.Text          as T
import           Data.Text.Template (Context)

-- | Create a 'Context' from 'Options'. Will raise error if the key was not found.
context' :: Options -> Context
context' assocs x = maybe err id . lookup x $ map (T.pack *** T.pack) $ Map.toList assocs
  where err = error $ "Could not find key: " ++ T.unpack x

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
        otherwise             = error $ "Could not find key: " ++ T.unpack x
