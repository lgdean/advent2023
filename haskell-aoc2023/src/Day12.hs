module Day12
    (
      howManyWays,
      doPart1,
--      doPart2
    ) where

import Data.List (group)
import Data.List.Split (splitOn)

-- or use two states and Nothing?
data SpringState = Operational | Broken | Unknown deriving (Eq, Show)

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
arrangementsFrom :: [Char] -> [Int] -> [[SpringState]]
arrangementsFrom pattern brokenGroupLengths =
  let allMatchingPattern = sequence (map (statesFor . parseState) pattern)
      rightOnesBroken poss = brokenGroupLengths == (map length $ filter ((==Broken) . head) $ group poss)
  in filter rightOnesBroken allMatchingPattern

statesFor :: SpringState -> [SpringState]
statesFor Broken = [Broken]
statesFor Operational = [Operational]
statesFor  _  = [Broken, Operational]

parseState :: Char -> SpringState
parseState '#' = Broken
parseState '.' = Operational
parseState '?' = Unknown
parseState  x  = error (x : " is not a valid state")
