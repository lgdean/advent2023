{-# LANGUAGE OverloadedStrings #-}
module Day16Spec (spec) where

import Test.Hspec

import Day16

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day16-example"
      doPart1 input `shouldBe` 46

    it "can solve Part 1" $ do
      input <- readFile "inputs/day16"
      doPart1 input `shouldBe` 7074

  describe "Part 2" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day16-example"
      doPart2 input `shouldBe` 51

    it "can solve Part 2" $ do
      input <- readFile "inputs/day16"
      doPart2 input `shouldBe` 7530 -- 6 seconds, may or may not ever improve it
