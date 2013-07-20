{-# LANGUAGE OverloadedStrings #-}

module Boilerplate.Flag
    (
      extractInitFlags
    ) where

import           Boilerplate.Types

extractInitFlags :: [Arg] -> InitFlags
extractInitFlags args = InitFlags { packageName = lookupPackageName
                                  , moduleName  = lookupModuleName
                                  , author      = lookupAuthor
                                  , email       = lookupEmail
                                  , repository  = lookupRepository
                                  , year        = lookupRepository
                                  }
  where
    lookupPackageName = lookup' "packageName"
    lookupModuleName  = lookup' "moduleName"
    lookupAuthor      = lookup' "author"
    lookupEmail       = lookup' "email"
    lookupRepository  = lookup' "repository"
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
defaultRepo = "git://github.com/fujimura/boilerplate-hspec.git"
