{-# LANGUAGE OverloadedStrings #-}

module Hi.Flag
    (
      extractOptions
    ) where

import           Data.Maybe (mapMaybe)
import           Hi.Types

-- | Extract 'Options' from given 'Option's.
extractOptions :: [Option] -> Either [Error] Options
extractOptions args = validateAll [(l, v) | (Arg l v) <- args]
  where
    validateAll :: Options -> Either [Error] Options
    validateAll values = case mapMaybe ($ values) validations of
                           []      -> Right values
                           errors  -> Left errors
    validations ::[Options -> Maybe String]
    validations = [ hasKey "packageName"
                  , hasKey "moduleName"
                  , hasKey "author"
                  , hasKey "email"
                  , hasKey "repository"
                  , hasKey "year"
                  ]
    hasKey :: String -> Options -> Maybe String
    hasKey k values = case lookup k values of
                        Just _  -> Nothing
                        Nothing -> Just $ "Could not find option: " ++ k
