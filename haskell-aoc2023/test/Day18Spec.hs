{-# LANGUAGE OverloadedStrings #-}
module Day18Spec (spec) where

import Test.Hspec

import Day18

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day18-example"
      doPart1 input `shouldBe` 62

    it "can solve Part 1" $ do
      input <- readFile "inputs/day18"
      doPart1 input `shouldBe` 48652

--  describe "Part 2" $ do
--    it "can handle given example" $ do
--      input <- readFile "inputs/day18-example"
--      doPart2 input `shouldBe` 952408144115
--
--    it "can solve Part 2" $ do
--      input <- readFile "inputs/day18"
--      doPart2 input `shouldBe` 0
