{-# LANGUAGE TupleSections #-}
module Day16
    (
      doPart1,
      doPart2
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
  in howManyTilesEnergized gridLayout initBeam

howManyTilesEnergized :: Map Coord Tile -> Beam -> Int
howManyTilesEnergized gridLayout initBeam =
  let
      firstBeamOrBeams = moveBeam gridLayout initBeam
      allMoves = iterate (moveBeams gridLayout) (Set.empty, firstBeamOrBeams)
      allPositions = Set.map fst $ fst (allMoves !! 1000)
      result = allPositions :: Set Coord
  in Set.size result

moveBeams :: TileLayout -> (Set Beam, Set Beam) -> (Set Beam, Set Beam)
moveBeams layout (seenSoFar, currBeams) =
  let nextBeams = Set.unions $ Set.map (moveBeam layout) currBeams
  in (Set.union seenSoFar currBeams, Set.difference nextBeams seenSoFar)

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

doPart2 :: [Char] -> Int
doPart2 input =
  let gridLayout = Map.map parseTile $ parseGrid input :: TileLayout
      maxX = length $ head $ lines input
      maxY = length $ lines input
      initBeams = concat [[((-1, y), R) | y <- [0 .. maxY-1]]
                         ,[((maxX, y), L) | y <- [0 .. maxY-1]]
                         ,[((x, -1), Down) | x <- [0 .. maxX-1]]
                         ,[((x, maxY), Up) | x <- [0 .. maxX-1]]
                         ]
      results = map (howManyTilesEnergized gridLayout) initBeams
  in maximum results
