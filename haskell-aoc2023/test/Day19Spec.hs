{-# LANGUAGE OverloadedStrings #-}
module Day19Spec (spec) where

import Test.Hspec

import Day19

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day19-example"
      doPart1 input `shouldBe` 19114

    it "can solve Part 1" $ do
      input <- readFile "inputs/day19"
      doPart1 input `shouldBe` 401674

  describe "Part 2" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day19-example"
      doPart2 input `shouldBe` 167409079868000

    it "can solve Part 2" $ do
      input <- readFile "inputs/day19"
      doPart2 input `shouldBe` 134906204068564
