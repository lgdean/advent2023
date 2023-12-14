module Day14
    (
      doPart1,
--      doPart2
    ) where

import Data.List (transpose)

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
