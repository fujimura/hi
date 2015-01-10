{-# LANGUAGE ScopedTypeVariables #-}

module FeatureSpec where

import qualified Hi.Cli              as Cli
import           Hi.Directory        (inDirectory)

import           Control.Applicative
import           Control.Exception   (catch, throwIO)
import           Data.Time.Calendar  (toGregorian)
import           Data.Time.Clock     (getCurrentTime, utctDay)
import           Data.Version        (showVersion)
import           SpecHelper
import           System.Directory    (doesDirectoryExist, doesFileExist,
                                      getCurrentDirectory)
import           System.Exit         (ExitCode (..))
import           System.FilePath     (joinPath, (</>))
import           System.IO           (stdout)
import           System.IO.Silently  (capture, hSilence)
import           System.Process      (readProcess, system)
import           Test.Hspec

import           Paths_hi            (version)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "with command line options" $ do
      let cmd = runWithCommandLineOptions [ "-p", packageName , "-m", moduleName ]
      around_ cmd features

    describe "with custom git config" $ do
      let cmd = runWithLocalGitConfig [ "-p", packageName , "-m", moduleName ]
      around_ cmd features

    describe "with config file" $ do
      let cmd = runWithConfigurationFile [ "-p", packageName , "-m", moduleName ]
      around_ cmd features

    describe "Only package name was given" $ do
      let cmd = runWithCommandLineOptions ["-p", "something-weird"]

      around_ cmd $ do
        it "should use Capitalized package name as module name" $ do
          doesDirectoryExist "something-weird/src/Something/Weird" `shouldReturn` True

    describe "Package name was given as an argument" $ do
      let cmd = runWithCommandLineOptions ["something-weird"]

      around_ cmd $ do
        it "should use Capitalized package name as module name" $ do
          doesDirectoryExist "something-weird/src/Something/Weird" `shouldReturn` True

   -- https://github.com/fujimura/hi/issues/48
    describe "with --module-name, not --moduleName" $ do
      let cmd = runWithConfigurationFile [ "-p", packageName , "--module-name", moduleName ]
      around_ cmd features

    describe "with --initialize-git-repository" $ do
      let cmd = runWithCommandLineOptions [ "--initialize-git-repository"
                                          , "-p"
                                          , packageName
                                          , "-m"
                                          , moduleName ]
      around_ cmd $ do
        it "should initialize it as git repository and make first commit" $ do
          inDirectory "./testapp" $ do
            readProcess "git" ["log", "-1", "--pretty=%s"] [] `shouldReturn` "Initial commit\n"

    describe "with --after-command='cabal sandbox init'" $ do
      let cmd = runWithCommandLineOptions [ "--after-command"
                                          , "cabal sandbox init"
                                          , "-p"
                                          , packageName
                                          ]
      around_ cmd $ do
        it "should initialize it as git repository and make first commit" $ do
          doesFileExist "testapp/cabal.sandbox.config" `shouldReturn` True

    describe "-v" $ do
      it "should show version number" $ do
        let handle ExitSuccess   = return ()
            handle e             = throwIO e

        (res,_) <- capture $ Cli.run ["-v"] `catch` handle
        res `shouldBe` (showVersion version) ++ "\n"

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
    root <- getCurrentDirectory
    inTestDirectory $ hSilence [stdout] $ do
      Cli.run $ opts ++ [ "-a", quote author
                        , "-e", quote email
                        , "-r", (root </> "test" </> "template")
                        ]
      action

runWithLocalGitConfig :: [String] -> IO () -> IO ()
runWithLocalGitConfig opts action = do
    root <- getCurrentDirectory
    inTestDirectory $ hSilence [stdout] $ do
        _ <- system $ "git init"
        _ <- system $ "git config user.name" ++ " " ++ quote author
        _ <- system $ "git config user.email" ++ " " ++ quote email
        Cli.run $ opts ++ [ "-r", (root </> "test" </> "template") ]
        action

runWithConfigurationFile :: [String] -> IO () -> IO ()
runWithConfigurationFile opts action = do
    root <- getCurrentDirectory
    inTestDirectory $ hSilence [stdout] $ do
        writeFile ".hirc" $ concatLines
            [ "author: " ++ author
            , "email: " ++ email
            ]
        pwd' <- getCurrentDirectory
        withEnv "HOME" pwd' $ do
          Cli.run $ opts ++ [ "--configuration-file", (joinPath [pwd', ".hirc"])
                            , "-r", (root </> "test" </> "template")
                            ]
          action

{-# ANN module "HLint: Redundant do" #-}
