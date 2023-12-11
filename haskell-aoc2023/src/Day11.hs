module Day11
    (
      doPart1,
--      doPart2
    ) where

import Data.List (transpose)
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
doubleAsNeeded [] = []
doubleAsNeeded (xs:rest) =
  if all (== '.') xs then xs : xs : doubleAsNeeded rest else xs : doubleAsNeeded rest


-- copied once again! then not quite used as such
_parseGrid :: String -> Map (Int, Int) Char
_parseGrid input =
  let rows = lines input
  in Map.unions $ zipWith parseRow [0..] rows

parseRow :: Int -> String -> Map (Int, Int) Char
parseRow n row =
  let coords = zip [0..] (repeat n)
  in Map.fromList $ zip coords row

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (x1, y1) (x2, y2) =
  abs (x1-x2) + abs (y1 -y2)
