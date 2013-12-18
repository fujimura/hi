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
data Arg = Version | Help | Val Label String deriving(Eq, Show)

-- | Run mode.
data Mode = ShowVersion | ShowHelp | Run deriving(Eq, Show)
