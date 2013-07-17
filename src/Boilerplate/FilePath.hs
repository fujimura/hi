module Boilerplate.FilePath
    (
      toDestionationPath
    , modulePath
    ) where

import           Boilerplate.Option   (Options)
import           Boilerplate.Template (untemplate)
import           Data.List
import           Data.List.Split      (splitOn)
import           Data.Map             ((!))
import           System.FilePath      (joinPath)

toDestionationPath :: Options -> FilePath -> FilePath
toDestionationPath options = rename1 . rename2 . untemplate
  where
    rename1 = replace "package-name" (options ! "packageName")
    rename2 = replace "ModuleName" (toDir $ options ! "moduleName")

modulePath :: Options -> FilePath
modulePath options = toDir $ options ! "moduleName"

toDir :: String -> String
toDir = joinPath . splitOn "."

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace a b = foldl1 (++) . intersperse b . splitOn a
