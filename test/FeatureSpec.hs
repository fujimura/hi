{-# LANGUAGE OverloadedStrings #-}

module FeatureSpec ( spec ) where

import           Control.Applicative
import           Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.List                  (intercalate)
import           Data.Time.Calendar         (toGregorian)
import           Data.Time.Clock            (getCurrentTime, utctDay)
import           Distribution.Hi.Directory  (inTemporaryDirectory)
import           Distribution.Hi.Version    (version)
import           Helper
import           System.Directory           (doesDirectoryExist, doesFileExist,
                                             getCurrentDirectory)
import           System.Process             (readProcess, system)

type Context = IO () -> IO ()

spec :: Spec
spec = do
    featureSpec "Run with command line options" runWithCommandLineOptions
    featureSpec "Run with command line option, without configuration file" runWithNoConfigurationFile
    featureSpec "Run with configuration file" runWithConfigurationFile

    describe "-v" $ do
      it "should show version number" $ do
        r <- readProcess "./dist/build/hi/hi" ["-v"] []
        r `shouldBe` version ++ "\n"

featureSpec :: String -> Context -> Spec
featureSpec desc setup = describe desc $ do
  let readResult = LBS.readFile

  describe "LICENSE" $ do
    it "should include author" $ setup $ do
      compiled <- readResult "LICENSE"
      compiled `shouldContain` "Fujimura Daisuke"

    it "should include year" $ setup $ do
      (year,_,_) <- (toGregorian . utctDay) <$> getCurrentTime
      compiled   <- readResult "LICENSE"
      compiled `shouldContain` (LBS.pack $ show year)

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

runWithConfigurationFile :: Context
runWithConfigurationFile cb = do
    let packageName = "testapp"
        moduleName  = "System.Awesome.Library"
        author      = "Fujimura Daisuke"
        email       = "me@fujimuradaisuke.com"
        fileName    = ".hirc"

    pwd <- getCurrentDirectory

    inTemporaryDirectory "hi-test" $ do
        LBS.writeFile fileName $ LBS.pack $ concatLines
            [ "packageName: " ++ packageName
            , "moduleName: " ++ moduleName
            , "author: " ++ author
            , "email: " ++ email
            , "repository: " ++ pwd ++ "/template"
            ]
        pwd' <- getCurrentDirectory
        _ <- system $ concat [ pwd ++ "/dist/build/hi/hi"
                             , " --configuration-file ", pwd' ++ "/" ++ fileName
                             ]
        cb
  where
    concatLines :: [String] -> String
    concatLines = intercalate "\n"

runWithNoConfigurationFile :: Context
runWithNoConfigurationFile cb = do
    let packageName = "testapp"
        moduleName  = "System.Awesome.Library"
        author      = quote "Fujimura Daisuke"
        email       = quote "me@fujimuradaisuke.com"

    pwd <- getCurrentDirectory

    inTemporaryDirectory "hi-test" $ do
        _ <- system $ concat [ pwd ++ "/dist/build/hi/hi"
                             , " -p ", packageName
                             , " -m ", moduleName
                             , " -a ", author
                             , " -e ", email
                             , " -r " ++ pwd ++ "/template"
                             -- .hirc doesn't exist because here is a new
                             -- temporary directory
                             , " --configuration-file " ++ ".hirc"
                             ]
        cb
  where
    quote s = "\"" ++ s ++ "\""

runWithCommandLineOptions :: Context
runWithCommandLineOptions cb = do
    let packageName = "testapp"
        moduleName  = "System.Awesome.Library"
        author      = quote "Fujimura Daisuke"
        email       = quote "me@fujimuradaisuke.com"

    pwd <- getCurrentDirectory

    inTemporaryDirectory "hi-test" $ do
        _ <- system $ concat [ pwd ++ "/dist/build/hi/hi"
                             , " -p ", packageName
                             , " -m ", moduleName
                             , " -a ", author
                             , " -e ", email
                             , " -r " ++ pwd ++ "/template"
                             , " --no-configuration-file"
                             ]
        cb
  where
    quote s = "\"" ++ s ++ "\""
