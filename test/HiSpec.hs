module HiSpec ( spec ) where

import           Hi            (process)

import           Control.Monad
import           Data.Maybe    (fromJust, isJust)
import           Test.Hspec

options :: [(String,String)]
options = [ ("packageName" ,"testapp")
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
        context ("Option \"" ++ option ++ "\" was given and it's in the template") $
          it "should be replaced with the value" $
            let files = process options [("dummy.template", "Foo $" ++ option ++ " bar, \n")] in
            (fromJust $ lookup "dummy" files) `shouldContain` (fromJust $ lookup option options)

      context "ModuleName was given in file path" $
        it "should be replaced with given value, replacing period with path separator" $
          let files = process [("moduleName", "Bar")] [("foo/ModuleName/File.hs.template", "module Foo\n")] in
          lookup "foo/Bar/File.hs" files `shouldSatisfy` isJust
