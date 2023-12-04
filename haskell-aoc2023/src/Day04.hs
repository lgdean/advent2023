module Day04
    (
      doPart1,
      doPart2
    ) where

import Data.List (intersect)
import Data.Map (Map, (!))
import qualified Data.Map.Strict as Map
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

doPart2 :: [Char] -> Int
doPart2 input =
  let cards = map parseCard $ lines input
      cardNum (c, _, _) = c
      allCardNums = map cardNum cards
      cardScores = Map.fromAscList $ zip allCardNums (map cardScore cards)
      howManyCards = Map.fromAscList $ zip allCardNums (repeat 1)
      finalCards = foldl (processCard cardScores) howManyCards allCardNums
  in sum finalCards

processCard :: Map Int Int -> Map Int Int -> Int -> Map Int Int
processCard cardScores howManyCards cardNum =
  let thisCardScore = cardScores ! cardNum
      nextN = take thisCardScore [cardNum+1..]
      howManyOfThisCard = howManyCards ! cardNum
  in foldl (flip (Map.adjust (+ howManyOfThisCard))) howManyCards nextN
