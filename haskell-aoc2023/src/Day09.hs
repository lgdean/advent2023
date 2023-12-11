module Day09
    (
      doPart1,
      doPart2
    ) where

import Lib (takeUntil)

type History = [Int]
type ExpandedHistory = [[Int]]

readHistories :: [Char] -> [History]
readHistories input =
  let allLines = lines input
      allHistories = map (map read . words) allLines :: [History]
  in allHistories

doPart1 :: [Char] -> Int
doPart1 input =
  let allHistories = readHistories input
      predictions = map predictNext allHistories
  in sum predictions

predictNext :: History -> Int
predictNext history =
  let rows = allRowsDownFrom history
  in foldr ((+) . last) 0 rows

doPart2 :: [Char] -> Int
doPart2 input =
  let allHistories = readHistories input
      predictions = map predictPrior allHistories
  in sum predictions

predictPrior :: History -> Int
predictPrior history =
  let rows = allRowsDownFrom history
  in foldr ((-) . head) 0 rows

allRowsDownFrom :: History -> ExpandedHistory
allRowsDownFrom row =
  let nextRowDownFrom xs = zipWith (-) (tail xs) xs
  in takeUntil (all (==0)) $ iterate nextRowDownFrom row
