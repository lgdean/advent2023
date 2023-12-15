{-# LANGUAGE OverloadedStrings #-}
module Day15Spec (spec) where

import Test.Hspec

import Day15

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can hash given example string HASH" $ do
      doPart1 "HASH" `shouldBe` 52

    it "can handle given example" $ do
      input <- readFile "inputs/day15-example"
      doPart1 input `shouldBe` 1320

    it "can solve Part 1" $ do
      input <- readFile "inputs/day15"
      doPart1 input `shouldBe` 514394
