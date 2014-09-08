module Hi.FilePath
    (
      rewritePath
    ) where

import           Hi.Template     (untemplate)
import           Hi.Types

import           Data.List
import           Data.List.Split (splitOn)
import           Data.Maybe      (fromJust)
import           System.FilePath (joinPath)

-- | Convert given path to the destination path, with given options.
rewritePath :: String -> String -> FilePath -> FilePath
rewritePath packageName moduleName =
    rename1 . rename2 . untemplate
  where
    rename1 = replace "package-name" packageName
    rename2 = replace "ModuleName" $ toDir moduleName

-- | Convert module name to path
-- @
-- toDir "Foo.bar" # => "Foo/Bar"
-- @
toDir :: String -> FilePath
toDir = joinPath . splitOn "."

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace a b = foldl1 (++) . intersperse b . splitOn a
