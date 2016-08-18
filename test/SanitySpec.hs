module SanitySpec where

import           Test.Hspec

spec :: Spec
spec = describe "when I have tests" $
    it "I have sanity" $ True `shouldBe` True
