{-# LANGUAGE OverloadedStrings #-}
module Day20Spec (spec) where

import Test.Hspec

import Day20

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day20-example-1"
      doPart1 input `shouldBe` 32000000

    it "can handle more interesting example" $ do
      input <- readFile "inputs/day20-example-2"
      doPart1 input `shouldBe` 11687500

    it "can solve Part 1" $ do
      input <- readFile "inputs/day20"
      doPart1 input `shouldBe` 898557000

--  describe "Part 2" $ do
--    it "can handle given example" $ do
--      input <- readFile "inputs/day20-example"
--      doPart2 input `shouldBe` FIXME
--
--    it "can solve Part 2" $ do
--      input <- readFile "inputs/day20"
--      doPart2 input `shouldBe` 0
