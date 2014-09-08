{-# LANGUAGE OverloadedStrings #-}

module Hi.Types
    ( Option(..)
    , File(..)
    , Files
    ) where

import Data.ByteString (ByteString)

data File = TemplateFile { getFilePath :: FilePath, getFileContents :: ByteString } | RegularFile { getFilePath :: FilePath, getFileContents :: ByteString }

type Files = [File]

data Option = Option
             { initializeGitRepository :: Bool
             , moduleName :: String
             , packageName :: String
             , author :: String
             , email :: String
             , year :: String
             , repository :: String
             } deriving (Eq,Ord,Show)
