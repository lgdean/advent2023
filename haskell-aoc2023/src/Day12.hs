module Day12
    (
      howManyWays,
      doPart1,
--      doPart2
    ) where

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
  let allMatchingPattern = sequence (map possibleStatesFor pattern)
      rightOnesBroken poss = brokenGroupLengths == (map length $ filter ((=='#') . head) $ group poss)
  in filter rightOnesBroken allMatchingPattern

possibleStatesFor :: Char -> [Char]
possibleStatesFor '#' = ['#']
possibleStatesFor '.' = ['.']
possibleStatesFor  _  = ['#','.']
