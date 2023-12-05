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

  describe "Part 2" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day05-example"
      doPart2 input `shouldBe` 46

--    it "can solve Part 2" $ do
--      input <- readFile "inputs/day05"
--      doPart2 input `shouldBe` 104070862 -- Finished in 1936.3873 seconds
