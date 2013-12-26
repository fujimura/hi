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
rewritePath :: Options -> FilePath -> FilePath
rewritePath flags =
    rename1 . rename2 . untemplate
  where
    rename1 = replace "package-name" $ fromJust $ lookup "packageName" flags
    rename2 = replace "ModuleName" (toDir . fromJust $ lookup "moduleName" flags)

-- | Convert module name to path
-- @
-- toDir "Foo.bar" # => "Foo/Bar"
-- @
toDir :: String -> FilePath
toDir = joinPath . splitOn "."

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace a b = foldl1 (++) . intersperse b . splitOn a
