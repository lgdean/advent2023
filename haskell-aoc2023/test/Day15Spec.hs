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

  describe "Part 2" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day15-example"
      doPart2 input `shouldBe` 145

    it "can hash a particular label from an early step" $ do
      doPart1 "hfx" `shouldBe` 6

    it "can solve Part 2" $ do
      input <- readFile "inputs/day15"
--      doPart2 input `shouldBe` 45990 -- wrong answer from a silly parse error
      doPart2 input `shouldBe` 236358
