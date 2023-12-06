{-# LANGUAGE OverloadedStrings #-}
module Day06Spec (spec) where

import Test.Hspec

import Day06

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      let input = [(7,9), (15,40), (30,200)]
      doPart1 input `shouldBe` 288

    it "can solve Part 1" $ do
      let input = [(46,347), (82,1522), (84,1406), (79,1471)]
      doPart1 input `shouldBe` 449550

  describe "Part 2" $ do
    it "can handle given example" $ do
      let input = [(71530,940200)]
      doPart1 input `shouldBe` 71503

    it "can solve Part 2" $ do
      let input = [(46828479,347152214061471)]
      doPart1 input `shouldBe` 28360140
