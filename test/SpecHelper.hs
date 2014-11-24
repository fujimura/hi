{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}

module SpecHelper where

import           Control.Exception   (bracket_, bracket)
import           Data.List           (intercalate)
import           System.Directory    (createDirectoryIfMissing,
                                      getCurrentDirectory,
                                      removeDirectoryRecursive,
                                      setCurrentDirectory)
import           System.Environment  (setEnv, lookupEnv, unsetEnv)

concatLines :: [String] -> String
concatLines = intercalate "\n"

withEnv :: String -> String -> IO a -> IO a
withEnv k v action = do
    bracket setup teardown (const action)
  where
    setup :: IO (Maybe String)
    setup = do
      mv <- lookupEnv k
      setEnv k v
      return mv
    teardown :: Maybe String -> IO ()
    teardown (Just _v) = setEnv k _v >> return ()
    teardown Nothing   = unsetEnv k >> return ()

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
