module Day21
    (
      doPart1,
--      doPart2
    ) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set


data Tile = Garden | Rock deriving (Eq, Show)
type Coord = (Int, Int)
type TileGrid = Map Coord Tile

doPart1 :: Int -> [Char] -> Int
doPart1 nSteps input =
  let charGrid = parseGrid input
      startPos = fst $ head $ dropWhile ((/= 'S') . snd) $ Map.toList charGrid :: Coord
      tileGrid = Map.map parseTile charGrid
      result = reachableFrom tileGrid nSteps (Set.singleton startPos) :: Set Coord
  in Set.size result

-- could improve by noting that n+1 is exactly n-1 plus new ones
-- BUT FIRST try brute force
reachableFrom :: TileGrid -> Int -> Set Coord -> Set Coord
reachableFrom _ 0 posns = posns
reachableFrom grid nSteps posns =
  let neighbors = Set.unions $ Set.map neighborCoords posns
      isGarden pos = grid Map.! pos /= Rock
      gardenNeighbors = Set.filter isGarden neighbors
  in reachableFrom grid (nSteps-1) gardenNeighbors

neighborCoords :: (Int, Int) -> Set (Int, Int)
neighborCoords (x,y) = Set.fromList [ (x+1,y), (x-1,y), (x,y+1), (x,y-1) ]

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