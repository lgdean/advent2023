{-# LANGUAGE OverloadedStrings #-}
module Day02Spec (spec) where

import Test.Hspec

import Day02

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day02-example"
      doPart1 input `shouldBe` 8

    it "can solve Part 1" $ do
      input <- readFile "inputs/day02"
      doPart1 input `shouldBe` 2771
