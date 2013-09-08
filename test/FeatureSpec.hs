{-# LANGUAGE OverloadedStrings #-}

module FeatureSpec ( spec ) where

import           Control.Applicative
import           Control.Exception          (bracket_)
import           Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.List                  (intercalate)
import           Data.Time.Calendar         (toGregorian)
import           Data.Time.Clock            (getCurrentTime, utctDay)
import           Hi.Version                 (version)
import           System.Directory           (createDirectoryIfMissing,
                                             doesDirectoryExist, doesFileExist,
                                             getCurrentDirectory,
                                             removeDirectoryRecursive,
                                             setCurrentDirectory)
import           System.Process             (readProcess, system)
import           Test.Hspec

type Context = IO () -> IO ()

spec :: Spec
spec = do
    describe "with command line options" $
      around runWithCommandLineOptions features

    describe "with command line options, without configuration file" $
      around runWithNoConfigurationFile features

    describe "with configuration file" $
      around runWithConfigurationFile features

    describe "-v" $ do
      it "should show version number" $ do
        r <- readProcess "./dist/build/hi/hi" ["-v"] []
        r `shouldBe` version ++ "\n"

features :: Spec
features = do
  let readResult = readFile

  describe "LICENSE" $ do
    it "should include author" $  do
      compiled <- readResult "LICENSE"
      compiled `shouldContain` "Fujimura Daisuke"

    it "should include year" $  do
      (year,_,_) <- (toGregorian . utctDay) <$> getCurrentTime
      compiled   <- readResult "LICENSE"
      compiled `shouldContain` show year

  describe "README.md" $ do
    it "should include name" $  do
      compiled <- readResult "README.md"
      compiled `shouldContain` "testapp"

  describe "module-name.cabal" $ do
    it "should include name" $  do
      compiled <- readResult "testapp.cabal"
      compiled `shouldContain` "testapp"

    it "should include author" $  do
      compiled <- readResult "testapp.cabal"
      compiled `shouldContain` "Fujimura Daisuke"

    it "should include email" $  do
      compiled <- readResult "testapp.cabal"
      compiled `shouldContain` "me@fujimuradaisuke.com"

    it "should include exposed-modules" $  do
      compiled <- readResult "testapp.cabal"
      compiled `shouldContain` "Exposed-Modules:      System.Awesome.Library"

    it "should include other-modules" $  do
      compiled <- readResult "testapp.cabal"
      compiled `shouldContain` "Other-Modules:        System.Awesome.Library.Internal"

  describe "directory" $ do
    it "should be made according to given module name" $  do
      doesDirectoryExist "src/System/Awesome/Library" `shouldReturn` True

  describe "Main module" $ do
    it "should be made" $  do
      doesFileExist "src/System/Awesome/Library.hs" `shouldReturn` True

    it "should include proper module name" $  do
      compiled <- readResult "src/System/Awesome/Library.hs"
      compiled `shouldContain` "module System.Awesome.Library"

  describe "Internal module" $ do
    it "should be made" $  do
      doesFileExist "src/System/Awesome/Library/Internal.hs" `shouldReturn` True

    it "should include proper module name" $  do
      compiled <- readResult "src/System/Awesome/Library/Internal.hs"
      compiled `shouldContain` "module System.Awesome.Library.Internal"

  describe "Spec.hs" $ do
    it "should be made" $  do
      doesFileExist "test/Spec.hs" `shouldReturn` True

    it "should include proper content" $  do
      compiled <- readResult "test/Spec.hs"
      compiled `shouldContain` "{-# OPTIONS_GHC -F -pgmF hspec-discover #-}"

  describe "Main spec" $ do
    it "should be made" $  do
      doesFileExist "test/System/Awesome/LibrarySpec.hs" `shouldReturn` True

    it "should include proper content" $  do
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

    inTestDirectory $ do
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

    inTestDirectory $ do
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

    inTestDirectory $ do
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

inTestDirectory :: (IO () -> IO ())
inTestDirectory cb = do
    pwd <- getCurrentDirectory
    let go    = do
            createDirectoryIfMissing True testDirectory
            setCurrentDirectory testDirectory
        flush = removeDirectoryRecursive testDirectory
        back  = setCurrentDirectory pwd
    bracket_ go (back >> flush) cb

testDirectory :: String
testDirectory = "test_project"
