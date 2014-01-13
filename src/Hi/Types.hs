{-# LANGUAGE OverloadedStrings #-}

module Hi.Types
    ( Label
    , Option(..)
    , Mode(..)
    , Files
    , Error
    ) where

type Files = [(FilePath, String)]

type Error = String

type Label = String

-- | Options
data Option = Version | Help | InitializeGitRepository | Arg Label String deriving(Eq, Show)

-- | Run mode.
data Mode = ShowVersion | ShowHelp | Run deriving(Eq, Show)
