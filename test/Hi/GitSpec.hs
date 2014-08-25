module Hi.GitSpec ( spec ) where

import           Hi.Git

import           Test.Hspec

spec :: Spec
spec = do
    let fullUrl = "git@github.com:fujimura/hi.git"
        shortUrl = "gh:fujimura/hi"
    describe "Hi.Git.expandUrl" $ do
      it "should do nothing with full git url" $ do
        expandUrl fullUrl `shouldBe` fullUrl
      it "should expand `gh:fujimura/hi` to `git@github.com:fujimura/hi.git`" $ do
        expandUrl shortUrl `shouldBe` fullUrl
