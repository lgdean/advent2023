{-# LANGUAGE OverloadedStrings #-}
module Day16Spec (spec) where

import Test.Hspec

import Day16

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day16-example"
      doPart1 input `shouldBe` 46

    it "can solve Part 1" $ do
      input <- readFile "inputs/day16"
      doPart1 input `shouldBe` 7074
