module Lib
    ( readIntLines
    , parseChunks
    , strip
    , mapSnd
    , bin2Int
    , divisibleBy
    , minAndMax
    , count
    , replace
    , takeUntil
    , fixedPoint
    , iterateUntilFixedPoint
    , takeUntilFixedPoint
    , applyNTimesDetectingCycle
    ) where

import Data.Char(digitToInt)
import Data.List (elemIndex)
import Data.List.Split (splitOn)
import qualified Data.Text as T

readIntLines :: [Char] -> [Int]
readIntLines = map read . lines

-- parse chunks of an input file, separated by empty lines
parseChunks :: ([String] -> a) -> String -> [a]
parseChunks chunkParser input =
  let allLines = lines input
      chunks = splitOn [""] allLines
  in map chunkParser chunks

strip :: String -> String
strip  = T.unpack . T.strip . T.pack

-- apparently not in Data.Tuple, ok
mapSnd :: (b -> c) -> [(a,b)] -> [(a,c)]
mapSnd f = map (\(x,y) -> (x, f y))

bin2Int :: String -> Int
bin2Int str = foldl (\acc n -> acc*2+n) 0 (map digitToInt str)

divisibleBy :: Integral a => a -> a -> Bool
divisibleBy candidate other = candidate `mod` other == 0

minAndMax :: (Ord a, Foldable t) => t a -> (a, a)
minAndMax xs = (minimum xs, maximum xs)

count :: Eq a => a -> [a] -> Int
count x xs = length $ filter (== x) xs

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\z -> if x == z then y else z)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ []          = []
takeUntil p (x:xs)
            | p x       = [x]
            | otherwise = x : takeUntil p xs

fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f initState =
  let nextState = f initState
  in if initState == nextState then nextState else fixedPoint f nextState

iterateUntilFixedPoint :: Eq a => (a -> a) -> a -> [a]
iterateUntilFixedPoint f initState =
  let nextState = f initState
  in initState : (if initState == nextState then [] else iterateUntilFixedPoint f nextState)

takeUntilFixedPoint :: Eq a => [a] -> [a]
takeUntilFixedPoint [] = []
takeUntilFixedPoint [x] = [x]
takeUntilFixedPoint (x:y:rest)
  | x == y    = [x]
  | otherwise = x : takeUntilFixedPoint (y:rest)

applyNTimesDetectingCycle :: (Eq a) => Int -> (a -> a) -> a -> a
applyNTimesDetectingCycle = applyNTimesDetectingCycle' []

-- could perhaps be made faster with use of Set
applyNTimesDetectingCycle' :: (Eq a) => [a] -> Int -> (a -> a) -> a -> a
applyNTimesDetectingCycle'   _   0 _ currState = currState
applyNTimesDetectingCycle' soFar n f currState =
  let beenSeen = elemIndex currState soFar
      nextState = f currState
      nToGo = n-1
  in case beenSeen of
    Nothing -> applyNTimesDetectingCycle' (currState:soFar) nToGo f nextState
    Just x -> (currState:soFar) !! (x - (nToGo `mod` (x+1)))
