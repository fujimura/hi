{-# LANGUAGE OverloadedStrings #-}

module HiSpec ( main, spec ) where

import           Hi                    (process)
import           Hi.Types

import           Data.ByteString.Char8 (unpack)
import           Data.Maybe            (fromJust, isJust)
import           Test.Hspec

main :: IO ()
main = hspec spec

options :: Option
options = Option { packageName    = "testapp"
                 , moduleName     = "System.Awesome.Library"
                 , directoryName  = "testapp"
                 , author         = "Fujimura Daisuke"
                 , email          = "me@fujimuradaisuke.com"
                 , templateSource = FromRepo "file://somewhere"
                 , year           = "2014"
                 , afterCommands  = []
                 }

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
      context "Option `packageName` was given and it's in the template" $
        it "should be replaced with the value" $
          let files = process options [TemplateFile "package-name/dummy.template" "Foo $packageName bar, \n"] in
          fromJust (lookupContent "testapp/dummy" files) `shouldContain` packageName options

      context "Option `moduleName` was given and it's in the template" $
        it "should be replaced with the value" $
          let files = process options [TemplateFile "package-name/dummy.template" "Foo $moduleName bar, \n"] in
          fromJust (lookupContent "testapp/dummy" files) `shouldContain` moduleName options

      context "Option `author` was given and it's in the template" $
        it "should be replaced with the value" $
          let files = process options [TemplateFile "package-name/dummy.template" "Foo $author bar, \n"] in
          fromJust (lookupContent "testapp/dummy" files) `shouldContain` author options

      context "Option `email` was given and it's in the template" $
        it "should be replaced with the value" $
          let files = process options [TemplateFile "package-name/dummy.template" "Foo $email bar, \n"] in
          fromJust (lookupContent "testapp/dummy" files) `shouldContain` email options

      context "`ModuleName` was given and `moduleName` is in the file path" $
        it "should be replaced with given value, replacing period with path separator" $
          let files = process (options { moduleName = "Bar"}) [TemplateFile "package-name/foo/ModuleName/File.hs.template" "module Foo\n"] in
          lookupContent "testapp/foo/Bar/File.hs" files `shouldSatisfy` isJust

      context "`/package-name` exists in template" $
        it "should generate files in package-name, not in package-name/package-name" $ do
          let files = process (options {packageName = "foo", moduleName = "Foo", directoryName = "baz"})
                              [TemplateFile "package-name/ModuleName/File.hs.template" "module Foo\n" ]

          lookupContent "baz/Foo/File.hs" files `shouldBe` Just "module Foo\n"

      describe "file without .template" $
        it "should be copied without substitution" $
          let files = process (options {moduleName = "Bar"}) [RegularFile "package-name/ModuleName/Foofile" "foo: $bar\n"] in
          lookupContent "testapp/Bar/Foofile" files `shouldBe` Just "foo: $bar\n"

      describe "Regular file and template file with same name" $
        it "should be copied without substitution" $
          let files = process (options {moduleName = "Bar"}) [RegularFile "package-name/Foofile" "foo: r\n", TemplateFile "package-name/Foofile" "foo: t\n"] in
          files `shouldBe` [TemplateFile "testapp/Foofile" "foo: t\n"]

      describe "Files in root directory" $
        it "should be copied" $
          let files = process (options {moduleName = "Bar"}) [RegularFile "Foofile" "foo: r\n", TemplateFile "package-name/Foofile" "foo: t\n"] in
          files `shouldBe` [TemplateFile "testapp/Foofile" "foo: t\n"]

      describe "Unknown variable in template" $
        it "should not be substituted" $
          let files = process (options {moduleName = "Bar"}) [TemplateFile "package-name/Foofile" "foo: $unknown\n"] in
          files `shouldBe` [TemplateFile "testapp/Foofile" "foo: $unknown\n"]

      describe "Directory name was specified" $
        it "should use it" $
          let files = process (options {moduleName = "Bar", directoryName = "baz"})
                              [TemplateFile "package-name/ModuleName/Foofile" "foo: x\n"] in
          files `shouldBe` [TemplateFile "baz/Bar/Foofile" "foo: x\n"]
