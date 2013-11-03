{-# LANGUAGE OverloadedStrings #-}

module Hi.Types
    (
      Flag
    , InitFlags
    , Label
    , Arg(..)
    , Mode(..)
    , Files
    , Error
    ) where

type Files = [(FilePath, String)]

type Flag = String

type InitFlags = [(String, String)]

type Error = String

type Label = String

-- | Arguments.
data Arg = Version | NoConfigurationFile | Val Label String deriving(Eq, Show)

-- | Run mode.
data Mode = ShowVersion | Run | RunWithNoConfigurationFile deriving(Eq, Show)
