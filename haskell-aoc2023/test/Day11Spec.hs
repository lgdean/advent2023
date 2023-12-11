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

  describe "Part 2" $ do
    it "can handle given example expanding 10x" $ do
      input <- readFile "inputs/day11-example"
      doPart2 10 input `shouldBe` 1030

    it "can handle given example expanding 100x" $ do
      input <- readFile "inputs/day11-example"
      doPart2 100 input `shouldBe` 8410

    it "can handle given example expanding 1000x" $ do
      input <- readFile "inputs/day11-example"
      doPart2 1000 input `shouldBe` 82210 -- according to 11-second slow code

    it "can solve Part 2" $ do
      input <- readFile "inputs/day11"
      doPart2 1000000 input `shouldBe` 710674907809
