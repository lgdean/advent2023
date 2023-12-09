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
  let nextRow = nextRowDownFrom row
      stopping = all (== 0) nextRow
  in if stopping
     then [row, nextRow]
     else row : allRowsDownFrom nextRow

nextRowDownFrom :: [Int] -> [Int]
nextRowDownFrom [] = error "did not consider this case"
nextRowDownFrom [_] = []
nextRowDownFrom (x:y:rest) = (y-x) : nextRowDownFrom (y:rest)
