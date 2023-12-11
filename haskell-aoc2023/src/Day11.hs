module Day11
    (
      doPart1,
      doPart2
    ) where

import Data.List (findIndices, transpose)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

type Coord = (Int, Int)

doPart1 :: [Char] -> Int
doPart1 = doPart2 2

pairsFrom :: [a] -> [(a, a)]
pairsFrom [] = []
pairsFrom [_] = []
pairsFrom (x:xs) = map (\y -> (x,y)) xs ++ pairsFrom xs


-- copied once again!
parseGrid :: String -> Map (Int, Int) Char
parseGrid input =
  let rows = lines input
  in Map.unions $ zipWith parseRow [0..] rows

parseRow :: Int -> String -> Map (Int, Int) Char
parseRow n row =
  let coords = zip [0..] (repeat n)
  in Map.fromList $ zip coords row

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (x1, y1) (x2, y2) =
  abs (x1-x2) + abs (y1 -y2)

doPart2 :: Int -> [Char] -> Int
doPart2 n input =
  let rows = lines input
      emptyRows = findIndices (all (== '.')) rows
      emptyCols = findIndices (all (== '.')) $ transpose rows
      -- not the most efficient, but it doesn't need to be
      newCoord (x, y) =
        (x + (n-1) * length (takeWhile (<x) emptyCols),
         y + (n-1) * length (takeWhile (<y) emptyRows))
      origGrid = parseGrid input
      origGalaxyCoords = Map.keys $ Map.filter (== '#') origGrid
      galaxyCoords = map newCoord origGalaxyCoords
      galaxyPairs = pairsFrom galaxyCoords
      distances = map (uncurry manhattanDistance) galaxyPairs
  in sum distances
