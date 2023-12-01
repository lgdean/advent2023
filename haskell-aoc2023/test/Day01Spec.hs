module Day01Spec where

import Test.Hspec

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can" $
      1 `shouldBe` (1::Int)
