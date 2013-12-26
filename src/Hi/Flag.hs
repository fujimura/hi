{-# LANGUAGE OverloadedStrings #-}

module Hi.Flag
    (
      validateOptions
    ) where

import           Data.Maybe (mapMaybe)
import           Hi.Types
import           Hi.Utils

-- | Extract 'Options' from given 'Option's.
validateOptions :: [Option] -> Either [Error] [Option]
validateOptions values = case mapMaybe ($ values) validations of
                       []      -> Right values
                       errors  -> Left errors
validations ::[[Option] -> Maybe String]
validations = [ hasKey "packageName"
              , hasKey "moduleName"
              , hasKey "author"
              , hasKey "email"
              , hasKey "repository"
              , hasKey "year"
              ]

hasKey :: String -> [Option] -> Maybe String
hasKey k options = case lookupArg k options of
                      Just _  -> Nothing
                      Nothing -> Just $ "Could not find option: " ++ k
