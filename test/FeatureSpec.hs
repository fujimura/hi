{-# LANGUAGE ScopedTypeVariables #-}

module FeatureSpec ( spec ) where

import qualified Hi.Cli              as Cli
import           Hi.Directory        (inDirectory)
import           Hi.Version          (version)

import           Control.Applicative
import           Control.Exception   (bracket_, catch, throwIO)
import           Data.Time.Calendar  (toGregorian)
import           Data.Time.Clock     (getCurrentTime, utctDay)
import           System.Directory    (createDirectoryIfMissing,
                                      doesDirectoryExist, doesFileExist,
                                      getCurrentDirectory,
                                      removeDirectoryRecursive,
                                      setCurrentDirectory)
import           System.Exit         (ExitCode (..))
import           System.IO           (stdout)
import           System.IO.Silently  (capture, hSilence)
import           System.Process      (readProcess, system)
import           Test.Hspec

spec :: Spec
spec = do
    describe "with command line options" $ do
      let cmd = runWithCommandLineOptions [ "-p", packageName , "-m", moduleName ]
      around cmd features

    describe "with custom git config" $ do
      let cmd = runWithLocalGitConfig [ "-p", packageName , "-m", moduleName ]
      around cmd features

    describe "Package name was omitted and module name was given" $ do
      let cmd = runWithCommandLineOptions ["-m", "Data.SomethingWeird"]

      around cmd $ do
        it "should use underscorized and hyphenized moudule name as package namee" $ do
          doesDirectoryExist "data-something-weird/src/Data/SomethingWeird" `shouldReturn` True

    describe "with --initialize-git-repository" $ do
      let cmd = runWithCommandLineOptions [ "--initialize-git-repository"
                                          , "-p"
                                          , packageName
                                          , "-m"
                                          , moduleName ]
      around cmd $ do
        it "should initialize it as git repository and make first commit" $ do
          inDirectory "./testapp" $ do
            readProcess "git" ["log", "-1", "--pretty=%s"] [] `shouldReturn` "Initial commit\n"

    describe "-v" $ do
      it "should show version number" $ do
        let handle ExitSuccess   = return ()
            handle e             = throwIO e

        (res,_) <- capture $ Cli.run ["-v"] `catch` handle
        res `shouldBe` version ++ "\n"

packageName, moduleName, author, email :: String
packageName = "testapp"
moduleName  = "System.Awesome.Library"
author      = "Fujimura Daisuke"
email       = "me@fujimuradaisuke.com"

features :: Spec
features = do

  describe "LICENSE" $ do
    it "should include author" $  do
      compiled <- readFile "testapp/LICENSE"
      compiled `shouldContain` "Fujimura Daisuke"

    it "should include year" $  do
      (year,_,_) <- (toGregorian . utctDay) <$> getCurrentTime
      compiled   <- readFile "testapp/LICENSE"
      compiled `shouldContain` show year

  describe "README.md" $ do
    it "should include name" $  do
      compiled <- readFile "testapp/README.md"
      compiled `shouldContain` "testapp"

  describe "module-name.cabal" $ do
    it "should include name" $  do
      compiled <- readFile "testapp/testapp.cabal"
      compiled `shouldContain` "testapp"

    it "should include author" $  do
      compiled <- readFile "testapp/testapp.cabal"
      compiled `shouldContain` "Fujimura Daisuke"

    it "should include email" $  do
      compiled <- readFile "testapp/testapp.cabal"
      compiled `shouldContain` "me@fujimuradaisuke.com"

    it "should include exposed-modules" $  do
      compiled <- readFile "testapp/testapp.cabal"
      compiled `shouldContain` "Exposed-Modules:      System.Awesome.Library"

    it "should include other-modules" $  do
      compiled <- readFile "testapp/testapp.cabal"
      compiled `shouldContain` "Other-Modules:        System.Awesome.Library.Internal"

  describe "directory" $ do
    it "should be made according to given module name" $  do
      doesDirectoryExist "testapp/src/System/Awesome/Library" `shouldReturn` True

  describe "Main module" $ do
    it "should be made" $  do
      doesFileExist "testapp/src/System/Awesome/Library.hs" `shouldReturn` True

    it "should include proper module name" $  do
      compiled <- readFile "testapp/src/System/Awesome/Library.hs"
      compiled `shouldContain` "module System.Awesome.Library"

  describe "Internal module" $ do
    it "should be made" $  do
      doesFileExist "testapp/src/System/Awesome/Library/Internal.hs" `shouldReturn` True

    it "should include proper module name" $  do
      compiled <- readFile "testapp/src/System/Awesome/Library/Internal.hs"
      compiled `shouldContain` "module System.Awesome.Library.Internal"

  describe "Spec.hs" $ do
    it "should be made" $  do
      doesFileExist "testapp/test/Spec.hs" `shouldReturn` True

    it "should include proper content" $  do
      compiled <- readFile "testapp/test/Spec.hs"
      compiled `shouldContain` "{-# OPTIONS_GHC -F -pgmF hspec-discover #-}"

  describe "Main spec" $ do
    it "should be made" $  do
      doesFileExist "testapp/test/System/Awesome/LibrarySpec.hs" `shouldReturn` True

    it "should include proper content" $  do
      compiled <- readFile "testapp/test/System/Awesome/LibrarySpec.hs"
      compiled `shouldContain` "module System.Awesome.LibrarySpec (main, spec) where"

  describe ".gitignore" $ do
    it "should be made" $  do
      doesFileExist "testapp/.gitignore" `shouldReturn` True

runWithCommandLineOptions :: [String] -> IO () -> IO ()
runWithCommandLineOptions opts action = do
    inTestDirectory $ hSilence [stdout] $ do
      Cli.run $ opts ++ [ "-a", quote author
                        , "-e", quote email
                        ]
      action

runWithLocalGitConfig :: [String] -> IO () -> IO ()
runWithLocalGitConfig opts action = do
    inTestDirectory $ hSilence [stdout] $ do
        _ <- system $ "git init"
        _ <- system $ "git config user.name" ++ " " ++ quote author
        _ <- system $ "git config user.email" ++ " " ++ quote email
        Cli.run $ opts ++ [ "-a", quote author
                          , "-e", quote email
                          ]
        action

inTestDirectory :: IO () -> IO ()
inTestDirectory action = do
    pwd <- getCurrentDirectory
    let go    = createDirectoryIfMissing True testDirectory >> setCurrentDirectory testDirectory
        flush = removeDirectoryRecursive testDirectory
        back  = setCurrentDirectory pwd
    bracket_ go (back >> flush) action

testDirectory :: String
testDirectory = "test_project"

quote :: String -> String
quote s = "\"" ++ s ++ "\""

{-# ANN module "HLint: Redundant do" #-}
