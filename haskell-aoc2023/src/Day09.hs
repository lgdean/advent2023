module Day09
    (
      doPart1,
--      doPart2
    ) where

doPart1 :: [Char] -> Int
doPart1 input =
  let allLines = lines input
      allHistories = map (map read . words) allLines :: [[Int]]
      predictions = map predictNext allHistories
  in sum predictions

-- not sure why we're doing this odd algorithm, but let's go for it
predictNext :: [Int] -> Int
predictNext history =
  let rows = allRowsDownFrom history
  in sum $ map last rows

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
