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

  describe "Part 2" $ do
    it "can do one cycle on given example -- just checking for bugs" $ do
      input <- readFile "inputs/day14-example"
      expected <- readFile "inputs/day14-after-1cycle"
      tiltCycle (lines input) `shouldBe` lines expected
      loadOnNorth (tiltCycle (lines input)) `shouldBe` 87

    it "can do three cycles on given example" $ do
      input <- readFile "inputs/day14-example"
      expected <- readFile "inputs/day14-after-3cycles"
      (tiltCycle . tiltCycle . tiltCycle) (lines input) `shouldBe` lines expected

    it "can handle given example" $ do
      input <- readFile "inputs/day14-example"
      --doPart2 input `shouldBe` 64
      doPart2 input `shouldBe` 0
