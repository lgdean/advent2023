{-# LANGUAGE TupleSections #-}
module Day18
    (
      doPart1,
--      doPart2
    ) where

import Data.List.Extra (groupOn)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set ()
import qualified Data.Set as Set

import Lib (count)

type Coord = (Int, Int)
data Dir = Up | Down | L | R deriving (Eq, Ord)

data TerrainState = Empty | Trench deriving (Eq, Show)
type TerrainLayout = Map Coord TerrainState -- could be a Set? we'll see
data DigState = Outside | EnteringTrench | Inside | ExitingTrench deriving (Eq, Show)

doPart1 :: [Char] -> Int
doPart1 input =
  let digPlan = map ((\(x,y,_) -> (x,y)) . parseLine) (lines input)
      origin = (0, 0)
      segments = digPerPlan origin digPlan
      allVisited = origin : concat segments
      endState = Map.fromList $ zip allVisited (repeat Trench)
  in digOutInterior endState

digOutInterior :: TerrainLayout -> Int
digOutInterior initSetup =
  let (minX, maxX) = minAndMax $ Set.map fst $ Map.keysSet initSetup
      (minY, maxY) = minAndMax $ Set.map snd $ Map.keysSet initSetup
      pointWithState p = (p, Map.findWithDefault Empty p initSetup)
      digIfInside p@(x,y) =
        case Map.findWithDefault Empty p initSetup of
          Trench -> Trench
          Empty ->
            let rowParts = groupOn snd $ map (pointWithState . (, y)) [x .. maxX+1] :: [[(Coord, TerrainState)]]
                vEdges = filter (isVerticalEdge initSetup) $ map (map fst) $ filter ((== Trench) . snd . head) rowParts
                nRight = length vEdges
            in if even nRight then Empty else Trench
      yRange = [minY .. maxY]
      pointsForRow y = map (, y) [minX .. maxX] :: [Coord]
      allCoords = map pointsForRow yRange
      endDugStates = map (map digIfInside) allCoords
  in sum $ map (count Trench) endDugStates

isVerticalEdge :: TerrainLayout -> [Coord] -> Bool
isVerticalEdge _ [] = error "an empty list is not an edge, why are you asking?"
isVerticalEdge _ [_] = True -- for now, assume we have already figure out it's Trench
isVerticalEdge layout coords =
  let (oneEndX, y) = head coords
      otherEndX = fst $ last coords
      valueOf p = Map.findWithDefault Empty p layout
      partOfAJog = valueOf (oneEndX, y+1) == valueOf (otherEndX, y-1)
  in partOfAJog

_showRow :: [TerrainState] -> String
_showRow ts =
  let showOne Empty = '.'
      showOne Trench = '#'
  in map showOne ts

digPerPlan :: Coord -> [(Dir, Int)] -> [[Coord]]
digPerPlan start = scanl (dig . last) [start]

dig :: Coord -> (Dir, Int) -> [Coord]
dig start (dir, howFar) =
  take howFar $ tail $ iterate (move dir) start

move :: Dir -> Coord -> Coord
move R (x,y) = (x+1,y)
move L (x,y) = (x-1,y)
move Up (x,y) = (x,y-1)
move Down (x,y) = (x,y+1)

parseLine :: String -> (Dir, Int, String)
parseLine line =
  let parts = words line
  in case parts of
    [d, n, s] -> (parseDir d, read n, filter (`notElem` "()") s)
    _         -> error ("cannot parse line parts: " ++ show parts)

parseDir :: String -> Dir
parseDir "R" = R
parseDir "L" = L
parseDir "U" = Up
parseDir "D" = Down
parseDir  x  = error ("unknown direction: " ++ x)

minAndMax :: (Ord a, Foldable t) => t a -> (a, a)
minAndMax xs = (minimum xs, maximum xs)
