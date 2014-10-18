{-# LANGUAGE OverloadedStrings #-}

module HiSpec ( main, spec ) where

import           Hi                    (process)
import           Hi.Types
import           Hi.Utils

import           Control.Monad
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Maybe            (fromJust, isJust)
import           Test.Hspec

main :: IO ()
main = hspec spec

toOption :: (Label, String) -> Option
toOption = uncurry Arg

options :: [Option]
options = map toOption [ (PackageName ,"testapp")
          , (ModuleName  ,"System.Awesome.Library")
          , (Author      ,"Fujimura Daisuke")
          , (Email       ,"me@fujimuradaisuke.com")
          , (ConfigFile  ,".hirc")
          , (Year        ,"2013")
          , (Repository  ,"file://somewhere")
          ]

lookupContent :: FilePath -> Files -> Maybe String
lookupContent _  [] = Nothing
lookupContent fp (f:fs) = if getFilePath f == fp
                           then Just $ stringifyContents f
                           else lookupContent fp fs

stringifyContents :: File -> String
stringifyContents = unpack . getFileContents

spec :: Spec
spec =
    describe "Hi.process" $ do
      forM_ [minBound .. maxBound] $ \(option) ->
        context ("Option `" ++ labelToTemplateKey option ++ "` was given and it's in the template") $
          it "should be replaced with the value" $
            let fileContents = pack $ "Foo $" ++ labelToTemplateKey option ++ " bar, \n"
                files = process options [TemplateFile "dummy.template" fileContents] in
            (fromJust $ lookupContent "dummy" files) `shouldContain` (fromJust $ lookupArg option options)

      context "`ModuleName` was given and `moduleName` is in the file path" $
        it "should be replaced with given value, replacing period with path separator" $
          let files = process (map toOption [(ModuleName, "Bar")]) [TemplateFile "foo/ModuleName/File.hs.template" "module Foo\n"] in
          lookupContent "foo/Bar/File.hs" files `shouldSatisfy` isJust

      describe "file without .template" $ do
        it "should be copied without substitution" $
          let files = process (map toOption [(ModuleName, "Bar")]) [RegularFile "ModuleName/Foofile" "foo: $bar\n"] in
          lookupContent "Bar/Foofile" files `shouldBe` Just "foo: $bar\n"
