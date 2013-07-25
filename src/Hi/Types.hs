{-# LANGUAGE OverloadedStrings #-}

module Hi.Types
    (
      Flag
    , InitFlags(..)
    , Label
    , Arg(..)
    , Mode(..)
    ) where

type Flag = String

data InitFlags =
    InitFlags { packageName :: !Flag
              , moduleName  :: !Flag
              , author      :: !Flag
              , email       :: !Flag
              , repository  :: !Flag
              , year        :: !Flag
              } deriving(Eq, Show)

type Label = String

-- | Arguments.
data Arg = Version | Val Label String deriving(Eq, Show)

-- | Run mode.
data Mode = ShowVersion | Run deriving(Eq, Show)
