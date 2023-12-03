{-# LANGUAGE OverloadedStrings #-}
module Day03Spec (spec) where

import Test.Hspec

import Day03

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day03-example"
      doPart1 input `shouldBe` 4361

    it "can solve Part 1" $ do
      input <- readFile "inputs/day03"
      doPart1 input `shouldBe` 535235
