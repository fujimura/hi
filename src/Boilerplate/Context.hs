module Boilerplate.Context
    (
      context
    ) where

import           Boilerplate.Option (Options)
import           Control.Arrow      ((***))
import qualified Data.Map           as Map
import qualified Data.Text          as T
import           Data.Text.Template (Context)

context :: Options -> Context
context assocs x = maybe err id . lookup x $ map (T.pack *** T.pack) $ Map.toList assocs
  where err = error $ "Could not find key: " ++ T.unpack x
