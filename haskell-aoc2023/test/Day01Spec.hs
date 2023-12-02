{-# LANGUAGE OverloadedStrings #-}
module Day01Spec (spec) where

import Test.Hspec

import Day01

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day01-example"
      doPart1 input `shouldBe` 142

    it "can solve Part 1" $ do
      input <- readFile "inputs/day01"
      doPart1 input `shouldBe` 53386

  describe "Part 2" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day01-example-words"
      doPart2 input `shouldBe` 281

    it "can solve Part 2" $ do
      input <- readFile "inputs/day01"
      doPart2 input `shouldBe` 53312
