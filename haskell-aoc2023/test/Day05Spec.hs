{-# LANGUAGE OverloadedStrings #-}
module Day05Spec (spec) where

import Test.Hspec

import Day05

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day05-example"
      doPart1 input `shouldBe` 35

    it "can solve Part 1" $ do
      input <- readFile "inputs/day05"
      doPart1 input `shouldBe` 324724204
