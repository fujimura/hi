module HiSpec ( spec ) where

import           Hi            (process)
import           Hi.Types
import           Hi.Utils

import           Control.Monad
import           Data.Maybe    (fromJust, isJust)
import           Test.Hspec

toOption :: (String, String) -> Option
toOption = uncurry Arg

options :: [Option]
options = map toOption [ ("packageName" ,"testapp")
          , ("moduleName"  ,"System.Awesome.Library")
          , ("author"      ,"Fujimura Daisuke")
          , ("email"       ,"me@fujimuradaisuke.com")
          , ("fileName"    ,".hirc")
          , ("year"        ,"2013")
          , ("repository"  ,"file://somewhere")
          ]

spec :: Spec
spec =
    describe "Hi.process" $ do
      forM_ ["packageName", "moduleName", "author", "email", "year"] $ \(option) ->
        context ("Option `" ++ option ++ "` was given and it's in the template") $
          it "should be replaced with the value" $
            let files = process options [("dummy.template", "Foo $" ++ option ++ " bar, \n")] in
            (fromJust $ lookup "dummy" files) `shouldContain` (fromJust $ lookupArg option options)

      context "`ModuleName` was given and `moduleName` is in the file path" $
        it "should be replaced with given value, replacing period with path separator" $
          let files = process (map toOption [("moduleName", "Bar")]) [("foo/ModuleName/File.hs.template", "module Foo\n")] in
          lookup "foo/Bar/File.hs" files `shouldSatisfy` isJust

      describe "file without .template" $ do
        it "should be copied without substitution" $
          let files = process (map toOption [("moduleName", "Bar")]) [("ModuleName/Foofile", "foo: $bar\n")] in
          lookup "Bar/Foofile" files `shouldBe` Just "foo: $bar\n"
