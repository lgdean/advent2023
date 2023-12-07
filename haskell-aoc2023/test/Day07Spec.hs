{-# LANGUAGE OverloadedStrings #-}
module Day07Spec (spec) where

import Test.Hspec

import Day07

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day07-example"
      doPart1 input `shouldBe` 6440

    it "can solve Part 1" $ do
      input <- readFile "inputs/day07"
      doPart1 input `shouldBe` 250254244
