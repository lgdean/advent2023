{-# LANGUAGE TupleSections #-}
module Day17
    (
      doPart1,
--      doPart2
    ) where

import Data.Char (digitToInt)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

type Coord = (Int, Int)
data Dir = Up | Down | L | R deriving (Eq, Ord)
--type Position = (Coord, Dir)

type BlockLayout = Map Coord Int

doPart1 :: [Char] -> Int
doPart1 input =
  let gridLayout = Map.map digitToInt $ parseGrid input :: BlockLayout
  in Map.size gridLayout

-- not sure it matters whether left or right tbh
turnFrom :: Dir -> [Dir]
turnFrom R = [Up, Down]
turnFrom L = [Up, Down]
turnFrom Down = [R, L]
turnFrom Up = [R, L]

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
