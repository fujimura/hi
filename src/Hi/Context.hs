module Hi.Context
    (
      context
    ) where

import           Hi.Types

import           Data.Map           ((!))
import qualified Data.Text          as T
import           Data.Text.Template (Context)

-- | Create a 'Context' from 'InitFlags Will raise error if the key was not found.
context :: InitFlags -> Context
context flags x = T.pack (flags !  T.unpack x)
