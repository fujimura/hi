{-# LANGUAGE OverloadedStrings #-}

module Hi.Types
    ( Label
    , Option(..)
    , Mode(..)
    , File(..)
    , Files
    , Error
    ) where

import Data.ByteString (ByteString)

data File = TemplateFile { getFilePath :: FilePath, getFileContents :: ByteString } | RegularFile { getFilePath :: FilePath, getFileContents :: ByteString }

type Files = [File]

type Error = String

type Label = String

-- | Options
data Option = Version | Help | InitializeGitRepository | Arg Label String deriving(Eq, Show)

-- | Run mode.
data Mode = ShowVersion | ShowHelp | Run deriving(Eq, Show)
