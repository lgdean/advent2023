module Day21
    (
      doPart1,
--      doPart2
    ) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet


data Tile = Garden | Rock deriving (Eq, Show)
type Coord = (Int, Int)
type TileGrid = (Int, Map Coord Tile)

doPart1 :: Int -> [Char] -> Int
doPart1 nSteps input =
  let charGrid = parseGrid input
      (x,y) = fst $ head $ dropWhile ((/= 'S') . snd) $ Map.toList charGrid
      tileGrid = Map.map parseTile $ Map.mapKeys (add (-x, -y)) charGrid
      result = reachableFrom (x, tileGrid) nSteps (HashSet.singleton (0,0)) :: HashSet Coord
  in HashSet.size result

-- TODO The puzzle input has the same property as the example:
-- no rocks around its outer edge. Could simplify properties of repetition?

reachableFrom :: TileGrid -> Int -> HashSet Coord -> HashSet Coord
reachableFrom _ 0 posns = posns
reachableFrom grid nSteps posns =
  reachableFrom' grid nSteps (HashSet.empty, posns) posns

-- improved by noting that n+1 is exactly n-1 plus new ones
reachableFrom' :: TileGrid -> Int -> (HashSet Coord, HashSet Coord) -> HashSet Coord -> HashSet Coord
reachableFrom' _ 0 (_, prevSet) _ = prevSet
reachableFrom' grid nSteps (theOneBefore, prevSet) posns =
  let neighbors = HashSet.unions $ map neighborCoords $ HashSet.toList posns
      newNeighbors = HashSet.difference neighbors theOneBefore
      gardenNeighbors = HashSet.filter (grid `isGardenAt`) newNeighbors
      newAndOld = HashSet.union theOneBefore gardenNeighbors
  in reachableFrom' grid (nSteps-1) (prevSet, newAndOld) gardenNeighbors

isGardenAt :: TileGrid -> Coord -> Bool
isGardenAt (offset, grid) (x,y) =
  let width = offset * 2 + 1 -- start position always in center
      adjusted n = (n + offset) `mod` width - offset
  in grid Map.! (adjusted x, adjusted y) /= Rock

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (a,b) (x,y) = (a+x, b+y)

neighborCoords :: (Int, Int) -> HashSet (Int, Int)
neighborCoords (x,y) = HashSet.fromList [ (x+1,y), (x-1,y), (x,y+1), (x,y-1) ]

-- copied once again! I did notice that the S is in the middle of the grid,
-- but for now will assume it's fine to do my usual thing of (0,0) top left.
parseGrid :: String -> Map (Int, Int) Char
parseGrid input =
  let rows = lines input
  in Map.unions $ zipWith parseRow [0..] rows

parseRow :: Int -> String -> Map (Int, Int) Char
parseRow n row =
  let coords = zip [0..] (repeat n)
  in Map.fromList $ zip coords row

parseTile :: Char -> Tile
parseTile 'S' = Garden
parseTile '.' = Garden
parseTile '#' = Rock
parseTile  c  = error (c : " is an unexpected Tile")
