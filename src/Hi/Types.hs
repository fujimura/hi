{-# LANGUAGE OverloadedStrings #-}

module Hi.Types
    ( Options
    , Label
    , Option(..)
    , Mode(..)
    , Files
    , Error
    ) where

type Files = [(FilePath, String)]

type Options = [(String, String)]

type Error = String

type Label = String

-- | Options
data Option = Version | Help | Arg Label String deriving(Eq, Show)

-- | Run mode.
data Mode = ShowVersion | ShowHelp | Run deriving(Eq, Show)
