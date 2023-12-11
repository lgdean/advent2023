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
      expandedHistories = map allRowsDownFrom allHistories
      predictions = map predictNext expandedHistories
  in sum predictions

predictNext :: ExpandedHistory -> Int
predictNext history =
  foldr ((+) . last) 0 history

doPart2 :: [Char] -> Int
doPart2 input =
  let allHistories = readHistories input
      expandedHistories = map allRowsDownFrom allHistories
      predictions = map predictPrior expandedHistories
  in sum predictions

predictPrior :: ExpandedHistory -> Int
predictPrior history =
  foldr ((-) . head) 0 history

allRowsDownFrom :: History -> ExpandedHistory
allRowsDownFrom row =
  let nextRowDownFrom xs = zipWith (-) (tail xs) xs
  in takeUntil (all (==0)) $ iterate nextRowDownFrom row
