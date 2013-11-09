module FeatureSpec ( spec ) where

import           Hi.Version          (version)

import           Control.Applicative
import           Control.Exception   (bracket_)
import           Data.List           (intercalate)
import           Data.Time.Calendar  (toGregorian)
import           Data.Time.Clock     (getCurrentTime, utctDay)
import           System.Directory    (createDirectoryIfMissing,
                                      doesDirectoryExist, doesFileExist,
                                      getCurrentDirectory,
                                      removeDirectoryRecursive,
                                      setCurrentDirectory)
import           System.Process      (readProcess, readProcessWithExitCode,
                                      system)
import           Test.Hspec

spec :: Spec
spec = do
    describe "with command line options" $
      around setupWithCommandLineOptions features

    describe "with configuration file" $
      around setupWithConfigurationFile features

    describe "-v" $ do
      it "should show version number" $ do
        r <- readProcess "./dist/build/hi/hi" ["-v"] []
        r `shouldBe` version ++ "\n"

    describe "with incomplete command line options" $ do
      it "should show error message" $ do
        (_,_,r) <- readProcessWithExitCode "./dist/build/hi/hi" ["-m", "Foo"] []
        r `shouldContain` "\n (Run with no arguments to see usage)"

packageName, moduleName, author, email, fileName :: String
packageName = "testapp"
moduleName  = "System.Awesome.Library"
author      = "Fujimura Daisuke"
email       = "me@fujimuradaisuke.com"
fileName    = ".hirc"

features :: Spec
features = do

  describe "LICENSE" $ do
    it "should include author" $  do
      compiled <- readFile "LICENSE"
      compiled `shouldContain` "Fujimura Daisuke"

    it "should include year" $  do
      (year,_,_) <- (toGregorian . utctDay) <$> getCurrentTime
      compiled   <- readFile "LICENSE"
      compiled `shouldContain` show year

  describe "README.md" $ do
    it "should include name" $  do
      compiled <- readFile "README.md"
      compiled `shouldContain` "testapp"

  describe "module-name.cabal" $ do
    it "should include name" $  do
      compiled <- readFile "testapp.cabal"
      compiled `shouldContain` "testapp"

    it "should include author" $  do
      compiled <- readFile "testapp.cabal"
      compiled `shouldContain` "Fujimura Daisuke"

    it "should include email" $  do
      compiled <- readFile "testapp.cabal"
      compiled `shouldContain` "me@fujimuradaisuke.com"

    it "should include exposed-modules" $  do
      compiled <- readFile "testapp.cabal"
      compiled `shouldContain` "Exposed-Modules:      System.Awesome.Library"

    it "should include other-modules" $  do
      compiled <- readFile "testapp.cabal"
      compiled `shouldContain` "Other-Modules:        System.Awesome.Library.Internal"

  describe "directory" $ do
    it "should be made according to given module name" $  do
      doesDirectoryExist "src/System/Awesome/Library" `shouldReturn` True

  describe "Main module" $ do
    it "should be made" $  do
      doesFileExist "src/System/Awesome/Library.hs" `shouldReturn` True

    it "should include proper module name" $  do
      compiled <- readFile "src/System/Awesome/Library.hs"
      compiled `shouldContain` "module System.Awesome.Library"

  describe "Internal module" $ do
    it "should be made" $  do
      doesFileExist "src/System/Awesome/Library/Internal.hs" `shouldReturn` True

    it "should include proper module name" $  do
      compiled <- readFile "src/System/Awesome/Library/Internal.hs"
      compiled `shouldContain` "module System.Awesome.Library.Internal"

  describe "Spec.hs" $ do
    it "should be made" $  do
      doesFileExist "test/Spec.hs" `shouldReturn` True

    it "should include proper content" $  do
      compiled <- readFile "test/Spec.hs"
      compiled `shouldContain` "{-# OPTIONS_GHC -F -pgmF hspec-discover #-}"

  describe "Main spec" $ do
    it "should be made" $  do
      doesFileExist "test/System/Awesome/LibrarySpec.hs" `shouldReturn` True

    it "should include proper content" $  do
      compiled <- readFile "test/System/Awesome/LibrarySpec.hs"
      compiled `shouldContain` "module System.Awesome.LibrarySpec (main, spec) where"

  describe ".gitignore" $ do
    it "should be made" $  do
      doesFileExist ".gitignore" `shouldReturn` True

setupWithConfigurationFile :: IO () -> IO ()
setupWithConfigurationFile action = do
    pwd <- getCurrentDirectory

    inTestDirectory $ do
        writeFile fileName $ concatLines
            [ "packageName: " ++ packageName
            , "moduleName: "  ++ moduleName
            , "author: "      ++ author
            , "email: "       ++ email
            , "repository: "  ++ "file://" ++ pwd ++ "/template"
            ]
        pwd' <- getCurrentDirectory
        _ <- system $ concat [ pwd ++ "/dist/build/hi/hi"
                             , " --configuration-file ", pwd' ++ "/" ++ fileName
                             ]
        action
  where
    concatLines :: [String] -> String
    concatLines = intercalate "\n"

setupWithCommandLineOptions :: IO () -> IO ()
setupWithCommandLineOptions action = do
    pwd <- getCurrentDirectory

    inTestDirectory $ do
        _ <- system $ concat [ pwd ++ "/dist/build/hi/hi"
                             , " -p ", packageName
                             , " -m ", moduleName
                             , " -a ", quote author
                             , " -e ", quote email
                             , " -r file://" ++ pwd ++ "/template"
                             ]
        action

inTestDirectory :: IO () -> IO ()
inTestDirectory action = do
    pwd <- getCurrentDirectory
    let go    = do
            createDirectoryIfMissing True testDirectory
            setCurrentDirectory testDirectory
        flush = removeDirectoryRecursive testDirectory
        back  = setCurrentDirectory pwd
    bracket_ go (back >> flush) action

testDirectory :: String
testDirectory = "test_project"

quote :: String -> String
quote s = "\"" ++ s ++ "\""
