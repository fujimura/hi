{-# LANGUAGE OverloadedStrings #-}

module Helper
    (
      module X
    , shouldContain
    ) where

import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import           Test.Hspec                 as X
import           Test.HUnit                 (assertBool)

shouldContain :: LBS.ByteString -> LBS.ByteString -> Expectation
shouldContain subject matcher = assertBool message (subject `contains` matcher)
    where
      s `contains` m = any (LBS.isPrefixOf m) $ LBS.tails s
      message  =
        "Expected \"" ++ LC8.unpack subject ++ "\" to contain \"" ++ LC8.unpack matcher ++ "\", but not"
