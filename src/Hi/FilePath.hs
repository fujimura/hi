module Hi.FilePath
    (
      rewritePath
    ) where

import           Hi.Template     (untemplate)

import           Data.List
import           Data.List.Split (splitOn)
import           System.FilePath (joinPath, splitPath)

-- | Convert given path to the destination path, with given options.
rewritePath :: String -> String -> FilePath -> FilePath
rewritePath packageName moduleName =
    replacePackageName . removeFirstPackageName . replaceModuleName . untemplate
  where
    replacePackageName = replace "package-name" packageName
    replaceModuleName  = replace "ModuleName" $ moduleNameToFilePath moduleName


-- | Remove first occurence of given subdirectory
--
-- >>> removeFirstPackageName "package-name/bar.hs"
-- "bar.hs"
--
-- >>> removeFirstPackageName "package-name/package-name.hs"
-- "package-name.hs"
--
-- >>> removeFirstPackageName "package-name.hs"
-- "package-name.hs"
--
removeFirstPackageName :: FilePath -> FilePath
removeFirstPackageName path = go $ splitPath path
  where
    go :: [FilePath] -> FilePath
    go [] = []
    go (x:xs)
      | x == "package-name/" = joinPath xs
      | otherwise            = joinPath $ x:xs

-- | Convert module name to path
--
-- >>> moduleNameToFilePath "Foo.Bar"
-- "Foo/Bar"
--
moduleNameToFilePath :: String -> FilePath
moduleNameToFilePath = joinPath . splitOn "."

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace a b = foldl1 (++) . intersperse b . splitOn a
