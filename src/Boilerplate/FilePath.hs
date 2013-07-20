module Boilerplate.FilePath
    (
      toDestionationPath
    , modulePath
    ) where

import           Boilerplate.Option   (InitFlags(..))
import           Boilerplate.Template (untemplate)
import           Data.List
import           Data.List.Split      (splitOn)
import           System.FilePath      (joinPath)

-- | Convert given path to the destination path, with given options.
toDestionationPath :: InitFlags -> FilePath -> FilePath
toDestionationPath InitFlags {moduleName=m, packageName=p} =
    rename1 . rename2 . untemplate
  where
    rename1 = replace "package-name" p
    rename2 = replace "ModuleName" (toDir m)

-- | Return path of module like `Foo/Bar`.
modulePath :: InitFlags -> FilePath
modulePath InitFlags {moduleName=m} = toDir m

-- @
-- toDir "Foo.bar" # => "Foo/Bar"
-- @
toDir :: String -> String
toDir = joinPath . splitOn "."

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace a b = foldl1 (++) . intersperse b . splitOn a
