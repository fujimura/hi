{-# LANGUAGE OverloadedStrings #-}

module Hi.Flag
    (
      extractInitFlags
    ) where

import qualified Data.Map   as M
import           Data.Maybe (mapMaybe)
import           Hi.Types

extractInitFlags :: [Arg] -> Either [Error] InitFlags
extractInitFlags args = validateAll $ M.fromList [(l, v) | (Val l v) <- args]
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
    hasKey k values = case lookup k $ M.toList values of
                        Just _  -> Nothing
                        Nothing -> Just $ "Could not find option: " ++ k

-- | Extract 'InitFlags' from a list of 'Arg'
extractInitFlags' :: [Arg] -> InitFlags'
extractInitFlags' args = InitFlags' { packageName = lookupPackageName
                                  , moduleName  = lookupModuleName
                                  , author      = lookupAuthor
                                  , email       = lookupEmail
                                  , repository  = lookupRepository
                                  , year        = lookupYear
                                  }
  where
    lookupPackageName = lookup' "packageName"
    lookupModuleName  = lookup' "moduleName"
    lookupAuthor      = lookup' "author"
    lookupEmail       = lookup' "email"
    lookupRepository  = lookup' "repository"
    lookupYear        = lookup' "year"
    lookup' label = case lookup label $ [(l, v) | (Val l v) <- args] of
                        Just v  -> v
                        Nothing -> if label == "repository"
                                     then defaultRepo
                                     else error $ errorMessageFor label
    errorMessageFor l = concat [ "Could not find option: "
                                , l
                                , "\n (Run with no arguments to see usage)"
                               ]

defaultRepo :: String
defaultRepo = "git://github.com/fujimura/hi-hspec.git"
