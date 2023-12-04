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
cardValue card =
  case cardScore card of
    0 -> 0
    x -> 2 ^ (x - 1)

cardScore :: (Int, [Int], [Int]) -> Int
cardScore (_, have, want) =
  length $ intersect have want

parseCard :: String -> (Int, [Int], [Int])
parseCard line =
  let parts = splitOneOf ":|" line
      cardNum = read $ head $ tail $ words $ head parts
      numsOnCard = map read $ words $ head $ tail parts
      winningNums = map read $ words $ head $ tail $ tail parts
  in (cardNum, numsOnCard, winningNums)
