module Day04
    (
      doPart1,
--      doPart2
    ) where

import Data.List (intersect)
import Data.List.Split(splitOneOf)

doPart1 :: [Char] -> Int
doPart1 input =
  let cards = map parseCard $ lines input
  in sum $ map cardValue cards

cardValue :: (Int, [Int], [Int]) -> Int
cardValue (_, have, want) =
  let commonNums = length $ intersect have want
  in case commonNums of
    0 -> 0
    _ -> 2 ^ (commonNums - 1)

parseCard :: String -> (Int, [Int], [Int])
parseCard line =
  let parts = splitOneOf ":|" line
      cardNum = read $ head $ tail $ words $ head parts
      numsOnCard = map read $ words $ head $ tail parts
      winningNums = map read $ words $ head $ tail $ tail parts
  in (cardNum, numsOnCard, winningNums)
