{-# LANGUAGE OverloadedStrings #-}
module Day21Spec (spec) where

import Test.Hspec

import Day21

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day21-example"
      doPart1 6 input `shouldBe` 16

    it "can solve Part 1" $ do
      input <- readFile "inputs/day21"
      doPart1 64 input `shouldBe` 3770
