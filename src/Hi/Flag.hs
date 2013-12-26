{-# LANGUAGE OverloadedStrings #-}

module Hi.Flag
    (
      extractInitFlags
    ) where

import           Data.Maybe (mapMaybe)
import           Hi.Types

-- | Extract 'InitFlags' from given 'Option's.
extractInitFlags :: [Option] -> Either [Error] InitFlags
extractInitFlags args = validateAll [(l, v) | (Val l v) <- args]
  where
    validateAll :: InitFlags -> Either [Error] InitFlags
    validateAll values = case mapMaybe ($ values) validations of
                           []      -> Right values
                           errors  -> Left errors
    validations ::[InitFlags -> Maybe String]
    validations = [ hasKey "packageName"
                  , hasKey "moduleName"
                  , hasKey "author"
                  , hasKey "email"
                  , hasKey "repository"
                  , hasKey "year"
                  ]
    hasKey :: String -> InitFlags -> Maybe String
    hasKey k values = case lookup k values of
                        Just _  -> Nothing
                        Nothing -> Just $ "Could not find option: " ++ k
