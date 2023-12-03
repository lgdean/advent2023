module Day03
    (
      doPart1,
      doPart2
    ) where

import Data.Char (isDigit)
import Data.List (intersect)
import Data.Map (Map, findWithDefault)
import qualified Data.Map.Strict as Map

doPart1 :: [Char] -> Int
doPart1 input =
  let
      grid = parseGrid input
      allNumbers = findNumbers $ lines input
      partNumbers = filter (hasNeighboringSymbol grid . snd) allNumbers
  in sum $ map fst partNumbers

findNumbers :: [String] -> [(Int, [(Int, Int)])]
findNumbers rows =
  concat $ zipWith findNumbersInRow [0..] rows

-- the number and its positions
findNumbersInRow :: Int -> String -> [(Int, [(Int, Int)])]
findNumbersInRow n line =
  let places = zip line [0..]
      numsFrom ((c,x):more) | isDigit c =
          let numParts = (c,x) : takeWhile (isDigit . fst) more
              num = read $ map fst numParts
          in (num, map (\(_,p) -> (p,n)) numParts) : numsFrom (dropWhile (isDigit . fst) more)
      numsFrom (_:more) = numsFrom more
      numsFrom [] = []
  in numsFrom places

-- copied from 2022 day 12; time for some lib functions?

parseGrid :: String -> Map (Int, Int) Char
parseGrid input =
  let rows = lines input
  in Map.unions $ zipWith parseRow [0..] rows

parseRow :: Int -> String -> Map (Int, Int) Char
parseRow n row =
  let coords = zip [0..] (repeat n)
  in Map.fromList $ zip coords row

-- copied from 2022 day 23, and this is the problem
-- ok but I do keep copying this one
add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (a,b) (x,y) = (a+x, b+y)

-- also this one, but there are 2 versions of it
neighborCoords :: (Int, Int) -> [(Int, Int)]
neighborCoords (x,y) =
  [add (x, y) (a, b) |
   a <- [- 1, 0, 1], b <- [- 1, 0, 1], (a, b) /= (0, 0)]

-- one could imagine preferring a Set, to avoid dupes
allNeighborCoords :: [(Int, Int)] -> [(Int, Int)]
allNeighborCoords = concatMap neighborCoords

isSymbol :: Char -> Bool
isSymbol '.' = False
isSymbol c = not $ isDigit c

-- TODO today's puzzle uses the term "adjacent"
hasNeighboringSymbol :: Map (Int, Int) Char -> [(Int, Int)] -> Bool
hasNeighboringSymbol grid positions =
  let containsSymbol pos = isSymbol $ findWithDefault '.' pos grid
  in any containsSymbol $ allNeighborCoords positions

doPart2 :: [Char] -> Int
doPart2 input =
  let
      grid = parseGrid input
      allNumbers = findNumbers $ lines input
      allStars = Map.filter (== '*') grid
      adjacentNumbers starPos =
          let neigbors = neighborCoords starPos
              isAdjacent (_, numPos) = not . null $ intersect neigbors numPos
          in map fst $ filter isAdjacent allNumbers
      gears = filter ((== 2) . length) $ map adjacentNumbers (Map.keys allStars)
  in sum $ map product gears
