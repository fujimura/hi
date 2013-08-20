{-# LANGUAGE OverloadedStrings #-}

module Hi.Flag
    (
      extractInitFlags
    ) where

import           Hi.Types

-- | Extract 'InitFlags' from a list of 'Arg'
extractInitFlags :: [Arg] -> InitFlags
extractInitFlags args = InitFlags { packageName = lookupPackageName
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
