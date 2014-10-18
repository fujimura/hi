{-# LANGUAGE OverloadedStrings #-}

module Hi.Types
    ( Label(..)
    , labelToTemplateKey
    , Option(..)
    , Mode(..)
    , File(..)
    , Files
    , Error
    ) where

import           Data.Char
import           Data.ByteString (ByteString)

data File = TemplateFile { getFilePath :: FilePath, getFileContents :: ByteString } | RegularFile { getFilePath :: FilePath, getFileContents :: ByteString }

type Files = [File]

type Error = String

data Label = Repository | PackageName | ModuleName | Author | Email | Year | ConfigFile | FileName
  deriving (Eq, Show)

labelToTemplateKey :: Label -> String
labelToTemplateKey label = case show label of
  x : xs -> toLower x : xs
  "" -> ""

-- | Options
data Option = Version | Help | InitializeGitRepository | Arg Label String
  deriving(Eq, Show)

-- | Run mode.
data Mode = ShowVersion | ShowHelp | Run deriving(Eq, Show)
