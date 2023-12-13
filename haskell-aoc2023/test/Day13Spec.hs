{-# LANGUAGE OverloadedStrings #-}
module Day13Spec (spec) where

import Test.Hspec

import Day13

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day13-example"
      doPart1 input `shouldBe` 405

    it "can do first chunk of Part 1" $ do
      input <- readFile "inputs/day13-first"
      doPart1 input `shouldBe` 200

    it "can solve Part 1" $ do
      input <- readFile "inputs/day13"
      doPart1 input `shouldBe` 36041

  describe "Part 2" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day13-example"
      doPart2 input `shouldBe` 400

    it "can solve Part 2" $ do
      input <- readFile "inputs/day13"
      doPart2 input `shouldBe` 35915
