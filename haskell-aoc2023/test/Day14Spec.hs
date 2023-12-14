{-# LANGUAGE OverloadedStrings #-}
module Day14Spec (spec) where

import Test.Hspec

import Day14

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day14-example"
      doPart1 input `shouldBe` 136

    it "can solve Part 1" $ do
      input <- readFile "inputs/day14"
      doPart1 input `shouldBe` 109755
