module Day09
    (
      doPart1,
      doPart2
    ) where

readHistories :: [Char] -> [[Int]]
readHistories input =
  let allLines = lines input
      allHistories = map (map read . words) allLines :: [[Int]]
  in allHistories

doPart1 :: [Char] -> Int
doPart1 input =
  let allHistories = readHistories input
      predictions = map predictNext allHistories
  in sum predictions

predictNext :: [Int] -> Int
predictNext history =
  let rows = allRowsDownFrom history
  in foldr ((+) . last) 0 rows

doPart2 :: [Char] -> Int
doPart2 input =
  let allHistories = readHistories input
      predictions = map predictPrior allHistories
  in sum predictions

predictPrior :: [Int] -> Int
predictPrior history =
  let rows = allRowsDownFrom history
  in foldr ((-) . head) 0 rows

allRowsDownFrom :: [Int] -> [[Int]]
allRowsDownFrom row =
  takeUntil (all (==0)) $ iterate nextRowDownFrom row

nextRowDownFrom :: [Int] -> [Int]
nextRowDownFrom xs = zipWith (-) (tail xs) xs

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ []          = []
takeUntil p (x:xs)
            | p x       = [x]
            | otherwise = x : takeUntil p xs
