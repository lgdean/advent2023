{-# LANGUAGE OverloadedStrings #-}
module Day21Spec (spec) where

import Test.Hspec

import Day21

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day21-example"
      doPart1 6 input `shouldBe` 16

    it "can solve Part 1" $ do
      input <- readFile "inputs/day21"
      doPart1 64 input `shouldBe` 3770

  describe "Part 2" $ do
    it "can handle first new given example" $ do
      input <- readFile "inputs/day21-example"
      doPart1 10 input `shouldBe` 50

--    it "can handle larger given example" $ do
--      input <- readFile "inputs/day21-example"
--      doPart1 100 input `shouldBe` 6536 -- under half a second!

--    it "can handle 5x that work, haha" $ do
--      input <- readFile "inputs/day21-example"
--      doPart1 500 input `shouldBe` 167004 -- 80 seconds... ok under 60 with HashSet

--    it "can handle even larger given example" $ do
--      input <- readFile "inputs/day21-example"
--      doPart1 1000 input `shouldBe` 668697 -- haven't tried it

--    it "can solve Part 2" $ do
--      input <- readFile "inputs/day21"
--      doPart1 26501365 input `shouldBe` 0
