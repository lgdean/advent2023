{-# LANGUAGE TupleSections #-}
module Day16
    (
      doPart1,
--      doPart2
    ) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Coord = (Int, Int)
data Dir = Up | Down | L | R deriving (Eq, Ord)
type Beam = (Coord, Dir)

-- 5 kinds of things? 3 kinds of things? 2 kinds plus Nothing? IDK
data Tile = Empty | FSMirror | BSMirror | VSplitter | HSplitter deriving Eq
type TileLayout = Map Coord Tile

doPart1 :: [Char] -> Int
doPart1 input =
  let gridLayout = Map.map parseTile $ parseGrid input :: TileLayout
      initBeam = ((-1, 0), R)
      firstBeamOrBeams = moveBeam gridLayout initBeam
      allMoves = iterate (Set.unions . Set.map (moveBeam gridLayout)) firstBeamOrBeams
      allPositions = take 1000 $ map (Set.map fst) allMoves
      result = Set.unions allPositions
  in Set.size result


moveBeam :: TileLayout -> Beam -> Set Beam
moveBeam layout (pos, dir) =
  let nextPos = move dir pos
  in case Map.lookup nextPos layout of
    Nothing -> Set.empty -- off edge of map, fine
    Just tile -> Set.fromList $ map (nextPos,) (changeDir tile dir)

changeDir :: Tile -> Dir -> [Dir]
changeDir Empty d = [d]
changeDir FSMirror R = [Up]
changeDir FSMirror Up = [R]
changeDir FSMirror L = [Down]
changeDir FSMirror Down = [L]
changeDir BSMirror R = [Down]
changeDir BSMirror Down = [R]
changeDir BSMirror L = [Up]
changeDir BSMirror Up = [L]
changeDir HSplitter Up = [L,R]
changeDir HSplitter Down = [L,R]
changeDir VSplitter L = [Up, Down]
changeDir VSplitter R = [Up, Down]
changeDir _ d = [d]

move :: Dir -> Coord -> Coord
move R (x,y) = (x+1,y)
move L (x,y) = (x-1,y)
move Up (x,y) = (x,y-1)
move Down (x,y) = (x,y+1)

-- copied once again!
parseGrid :: String -> Map (Int, Int) Char
parseGrid input =
  let rows = lines input
  in Map.unions $ zipWith parseRow [0..] rows

parseRow :: Int -> String -> Map (Int, Int) Char
parseRow n row =
  let coords = zip [0..] (repeat n)
  in Map.fromList $ zip coords row

parseTile :: Char -> Tile
parseTile '.' = Empty
parseTile '/' = FSMirror
parseTile '\\' = BSMirror
parseTile '-' = HSplitter
parseTile '|' = VSplitter
parseTile  x  = error ("could not parse tile: " ++ [x])
