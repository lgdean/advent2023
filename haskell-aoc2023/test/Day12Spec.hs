{-# LANGUAGE OverloadedStrings #-}
module Day12Spec (spec) where

import Test.Hspec

import Day12

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can count fully-known case" $ do
      howManyWays "#.#.### 1,1,3" `shouldBe` 1

    it "can count unknown but fully-determined case" $ do
      howManyWays "???.### 1,1,3" `shouldBe` 1

    it "can handle given example" $ do
      input <- readFile "inputs/day12-example"
      doPart1 input `shouldBe` 21

    it "can solve Part 1" $ do
      input <- readFile "inputs/day12"
      doPart1 input `shouldBe` 6827 -- works, but took 28 seconds
