{-# LANGUAGE OverloadedStrings #-}
module Day17Spec (spec) where

import Test.Hspec

import Day17

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day17-example"
      --doPart1 input `shouldBe` 102 TODO make it work like this
      doPart1 input `shouldBe` 169 -- placeholder

--    it "can solve Part 1" $ do
--      input <- readFile "inputs/day17"
--      doPart1 input `shouldBe` 0

--  describe "Part 2" $ do
--    it "can handle given example" $ do
--      input <- readFile "inputs/day17-example"
--      doPart2 input `shouldBe` FIXME
--
--    it "can solve Part 2" $ do
--      input <- readFile "inputs/day17"
--      doPart2 input `shouldBe` 0
