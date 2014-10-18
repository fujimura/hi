module Hi.Utils where

import           Hi.Types

lookupArg :: Label -> [Option] -> Maybe String
lookupArg key xs = lookup key [(k,v) | (Arg k v) <- xs]
