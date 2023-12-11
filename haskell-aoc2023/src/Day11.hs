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
doPart1 input =
  let rows = lines input
      expandedRows = doubleAsNeeded rows
      cols = transpose expandedRows
      expandedCols = doubleAsNeeded cols
      newGridInput = transpose expandedCols
      grid = Map.unions $ zipWith parseRow [0..] newGridInput
      galaxyCoords = Map.keys $ Map.filter (== '#') grid
      galaxyPairs = pairsFrom galaxyCoords
      distances = map (uncurry manhattanDistance) galaxyPairs
  in sum distances

pairsFrom :: [a] -> [(a, a)]
pairsFrom [] = []
pairsFrom [_] = []
pairsFrom (x:xs) = map (\y -> (x,y)) xs ++ pairsFrom xs

doubleAsNeeded :: [[Char]] -> [[Char]]
doubleAsNeeded = expandAsNeeded 2


-- copied once again! then not quite used as such
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
      origGrid = parseGrid input
      origGalaxyCoords = Map.keys $ Map.filter (== '#') origGrid
      emptyRows = findIndices (all (== '.')) rows
      emptyCols = findIndices (all (== '.')) $ transpose rows
      -- not the most efficient, but it doesn't need to be
      newCoord (x, y) =
        (x + (n-1) * length (takeWhile (<x) emptyCols),
         y + (n-1) * length (takeWhile (<y) emptyRows))
      galaxyCoords = map newCoord origGalaxyCoords
      galaxyPairs = pairsFrom galaxyCoords
      distances = map (uncurry manhattanDistance) galaxyPairs
  in sum distances

expandAsNeeded :: Int -> [[Char]] -> [[Char]]
expandAsNeeded _ [] = []
expandAsNeeded n (xs:rest) =
  (if all (== '.') xs then replicate n xs else [xs]) ++ expandAsNeeded n rest
