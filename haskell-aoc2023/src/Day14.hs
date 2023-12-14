module Day14
    (
      doPart1,
      loadOnNorth,
      tiltCycle,
      doPart2
    ) where

import Data.List (elemIndex, transpose)

doPart1 :: [Char] -> Int
doPart1 input =
  let rows = lines input
      cols = transpose rows
      tiltedCols = map (tiltTowardBegin []) cols
      countCol col = sum $ map fst $ filter ((== 'O') . snd) $ zip [1..] $ reverse col
  in sum $ map countCol tiltedCols

tiltTowardBegin :: [Char] -> [Char] -> [Char]
tiltTowardBegin acc ('O':rest) = tiltTowardBegin ('O':acc) rest
tiltTowardBegin acc ('.':rest) = tiltTowardBegin (acc ++ ".") rest
tiltTowardBegin acc ('#':rest) = acc ++ "#" ++ tiltTowardBegin [] rest
tiltTowardBegin  _ ( _ :_rest) = error "wat"
tiltTowardBegin acc [] = acc

loadOnNorth :: [[Char]] -> Int
loadOnNorth rows =
  let cols = transpose rows
      countCol col = sum $ map fst $ filter ((== 'O') . snd) $ zip [1..] $ reverse col
  in sum $ map countCol cols

doPart2 :: [Char] -> Int
doPart2 input =
  let rows = lines input
  in loadOnNorth $ applyNTimesDetectingCycle 1000000000 tiltCycle rows

tiltCycle :: [[Char]] -> [[Char]]
tiltCycle rows =
  let tiltedNorth = tiltAllTowardBegin $ transpose rows
      tiltedWest = tiltAllTowardBegin $ transpose tiltedNorth
      tiltedSouth = tiltAllTowardEnd $ transpose tiltedWest
      tiltedEast = tiltAllTowardEnd $ transpose tiltedSouth
  in tiltedEast

tiltAllTowardBegin :: [[Char]] -> [[Char]]
tiltAllTowardBegin = map (tiltTowardBegin [])

tiltAllTowardEnd :: [[Char]] -> [[Char]]
tiltAllTowardEnd = map (reverse . tiltTowardBegin [] . reverse)

applyNTimesDetectingCycle :: (Eq a) => Int -> (a -> a) -> a -> a
applyNTimesDetectingCycle = applyNTimesDetectingCycle' []

applyNTimesDetectingCycle' :: (Eq a) => [a] -> Int -> (a -> a) -> a -> a
applyNTimesDetectingCycle'   _   0 _ currState = currState
applyNTimesDetectingCycle' soFar n f currState =
  let beenSeen = elemIndex currState soFar
      nextState = f currState
      nToGo = n-1
  in case beenSeen of
    Nothing -> applyNTimesDetectingCycle' (currState:soFar) nToGo f nextState
    Just x -> (currState:soFar) !! (x - (nToGo `mod` (x+1)))
