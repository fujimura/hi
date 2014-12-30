{-# LANGUAGE OverloadedStrings #-}

module Hi.Types
    ( Option(..)
    , File(..)
    , Files
    , TemplateSource (..)
    ) where

import Data.ByteString (ByteString)

data File = TemplateFile { getFilePath :: FilePath, getFileContents :: ByteString } |
            RegularFile  { getFilePath :: FilePath, getFileContents :: ByteString } deriving (Show)

data TemplateSource = FromRepo String deriving (Eq,Ord,Show)

type Files = [File]

data Option = Option
             { initializeGitRepository :: Bool
             , moduleName :: String
             , packageName :: String
             , author :: String
             , email :: String
             , year :: String
             , templateSource :: TemplateSource
             } deriving (Eq,Ord,Show)
