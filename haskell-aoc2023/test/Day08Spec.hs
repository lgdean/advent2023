{-# LANGUAGE OverloadedStrings #-}
module Day08Spec (spec) where

import Test.Hspec

import Day08

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day08-example"
      doPart1 input `shouldBe` 2

    it "can solve Part 1" $ do
      input <- readFile "inputs/day08"
      doPart1 input `shouldBe` 22411

  describe "Part 2" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day08-example-ghost"
      doPart2 input `shouldBe` 6

    it "can solve Part 2" $ do
      input <- readFile "inputs/day08"
      doPart2 input `shouldBe` 11188774513823
