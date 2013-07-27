{-# LANGUAGE OverloadedStrings #-}

module FeatureSpec ( spec ) where

import           Distribution.Hi.Directory      (inTemporaryDirectory)
import qualified Distribution.Hi.Version
import           Control.Applicative
import           Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Helper
import           System.Directory           (doesDirectoryExist, doesFileExist,
                                             getCurrentDirectory)
import           System.Process             (readProcess, system)

spec :: Spec
spec = do
  let setup      = withCompiledFile
      readResult = LBS.readFile

  describe "LICENSE" $ do
    it "should include author" $ setup $ do
      compiled <- readResult "LICENSE"
      compiled `shouldContain` "Fujimura Daisuke"

  describe "README.md" $ do
    it "should include name" $ setup $ do
      compiled <- readResult "README.md"
      compiled `shouldContain` "testapp"

  describe "module-name.cabal" $ do
    it "should include name" $ setup $ do
      compiled <- readResult "testapp.cabal"
      compiled `shouldContain` "testapp"

    it "should include author" $ setup $ do
      compiled <- readResult "testapp.cabal"
      compiled `shouldContain` "Fujimura Daisuke"

    it "should include email" $ setup $ do
      compiled <- readResult "testapp.cabal"
      compiled `shouldContain` "me@fujimuradaisuke.com"

    it "should include exposed-modules" $ setup $ do
      compiled <- readResult "testapp.cabal"
      compiled `shouldContain` "Exposed-Modules:      System.Awesome.Library"

    it "should include other-modules" $ setup $ do
      compiled <- readResult "testapp.cabal"
      compiled `shouldContain` "Other-Modules:        System.Awesome.Library.Internal"

  describe "directory" $ do
    it "should be made according to given module name" $ setup $ do
      doesDirectoryExist "src/System/Awesome/Library" `shouldReturn` True

  describe "Main module" $ do
    it "should be made" $ setup $ do
      doesFileExist "src/System/Awesome/Library.hs" `shouldReturn` True

    it "should include proper module name" $ setup $ do
      compiled <- readResult "src/System/Awesome/Library.hs"
      compiled `shouldContain` "module System.Awesome.Library"

  describe "Internal module" $ do
    it "should be made" $ setup $ do
      doesFileExist "src/System/Awesome/Library/Internal.hs" `shouldReturn` True

    it "should include proper module name" $ setup $ do
      compiled <- readResult "src/System/Awesome/Library/Internal.hs"
      compiled `shouldContain` "module System.Awesome.Library.Internal"

  describe "Spec.hs" $ do
    it "should be made" $ setup $ do
      doesFileExist "test/Spec.hs" `shouldReturn` True

    it "should include proper content" $ setup $ do
      compiled <- readResult "test/Spec.hs"
      compiled `shouldContain` "{-# OPTIONS_GHC -F -pgmF hspec-discover #-}"

  describe "Main spec" $ do
    it "should be made" $ setup $ do
      doesFileExist "test/System/Awesome/LibrarySpec.hs" `shouldReturn` True

    it "should include proper content" $ setup $ do
      compiled <- readResult "test/System/Awesome/LibrarySpec.hs"
      compiled `shouldContain` "module Test.System.Awesome.LibrarySpec (main, spec) where"

  describe "-v" $ do
    it "should show version number" $ do
      r <- LBS.pack <$> readProcess ("./dist/build/hi/hi") ["-v"] []
      r `shouldContain` LBS.pack Distribution.Hi.Version.version

withCompiledFile :: IO a -> IO a
withCompiledFile cb = do
    pwd <- getCurrentDirectory
    inTemporaryDirectory "hi-test" $ do
        _ <- system $ concat
          [ pwd ++ "/dist/build/hi/hi"
          , " -p testapp"
          , " -m System.Awesome.Library"
          , " -a \"Fujimura Daisuke\""
          , " -e \"me@fujimuradaisuke.com\""
          , " -r " ++ pwd ++ "/template"
          ]
        cb
