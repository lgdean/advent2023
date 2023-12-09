{-# LANGUAGE OverloadedStrings #-}
module Day09Spec (spec) where

import Test.Hspec

import Day09

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day09-example"
      doPart1 input `shouldBe` 114

    it "can solve Part 1" $ do
      input <- readFile "inputs/day09"
      doPart1 input `shouldBe` 1762065988

  describe "Part 2" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day09-example"
      doPart2 input `shouldBe` 2

    it "can solve Part 2" $ do
      input <- readFile "inputs/day09"
      doPart2 input `shouldBe` 1066
