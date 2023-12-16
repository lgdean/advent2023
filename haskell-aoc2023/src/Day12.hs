module Day12
    (
      howManyWays,
      doPart1,
      doPart2
    ) where

import Data.List (group, intercalate)
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
  in howManyConsistentWith brokenGroups $ map parseState pattern

-- first, try brute force
_arrangementsFrom :: [Char] -> [Int] -> [[SpringState]]
_arrangementsFrom pattern brokenGroupLengths =
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

-- may need to memoize here, or take some entirely different approach
howManyConsistentWith :: [Int] -> [SpringState] -> Int
howManyConsistentWith [] pattern = if any (== Broken) pattern then 0 else 1
howManyConsistentWith (_:_) []   = 0 -- handled separately to satisfy a warning
howManyConsistentWith brokenRanges pattern
  | sum brokenRanges + length brokenRanges - 1 > length pattern = 0
howManyConsistentWith (n:restRanges) pattern@(Broken:_) =
  let nextN = take n pattern
      couldAllBeBroken = not $ any (== Operational) nextN && length nextN == n
      followingCouldBeOperational = length pattern <= n || pattern !! n /= Broken
      -- there are probably simpler ways to write that logic!
  in if couldAllBeBroken && followingCouldBeOperational
     then howManyStartingNonBroken restRanges (drop n pattern)
     else 0
howManyConsistentWith brokenRanges (Operational:restPattern) =
  howManyConsistentWith brokenRanges restPattern
howManyConsistentWith brokenRanges (Unknown:restPattern) =
  -- may need to do logic to optimize this case
  sum $ map (howManyConsistentWith brokenRanges) (map (:restPattern) (statesFor Unknown))

howManyStartingNonBroken :: [Int] -> [SpringState] -> Int
howManyStartingNonBroken []          [] = 1 -- fine to have broken spring at very end of list
howManyStartingNonBroken  _  (Broken:_) = 0
howManyStartingNonBroken [] (_:tailPattern) = howManyConsistentWith [] tailPattern -- or could inline
howManyStartingNonBroken ranges pattern = howManyConsistentWith ranges (tail pattern)

doPart2 :: [Char] -> Int
doPart2 input =
  sum $ map howManyWays2 $ lines input

howManyWays2 :: String -> Int
howManyWays2 row =
  let parts = words row
      brokenGroups = map read $ splitOn "," $ last parts
      pattern = head parts
      repeatedPattern = intercalate "?" $ take 5 $ repeat pattern
      times5 = concat . take 5 . repeat
  in howManyConsistentWith (times5 brokenGroups) $ map parseState repeatedPattern
