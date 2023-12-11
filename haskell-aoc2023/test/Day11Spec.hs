{-# LANGUAGE OverloadedStrings #-}
module Day11Spec (spec) where

import Test.Hspec

import Day11

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day11-example"
      doPart1 input `shouldBe` 374

    it "can solve Part 1" $ do
      input <- readFile "inputs/day11"
      doPart1 input `shouldBe` 9329143
