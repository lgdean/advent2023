module Day12
    (
      howManyWays,
      doPart1,
--      doPart2
    ) where

import Control.Monad (replicateM)
import Data.List (group)
import Data.List.Split (splitOn)

doPart1 :: [Char] -> Int
doPart1 input =
  sum $ map howManyWays $ lines input

howManyWays :: String -> Int
howManyWays row =
  let parts = words row
      brokenGroups = map read $ splitOn "," $ last parts
      pattern = head parts
  in length $ arrangementsFrom pattern brokenGroups

-- first, try brute force
arrangementsFrom :: [Char] -> [Int] -> [[Char]]
arrangementsFrom pattern brokenGroupLengths =
  let allPossible = replicateM (length pattern) ['#','.']
      allMatchingPattern = filter (couldBe pattern) allPossible
      rightOnesBroken poss = brokenGroupLengths == (map length $ filter ((=='#') . head) $ group poss)
  in filter rightOnesBroken allMatchingPattern

couldBe :: String -> String -> Bool
couldBe pattern arr =
  all (\(x, y) -> x == '?' || x == y) $ zip pattern arr
