module Day15
    (
      doPart1,
--      loadOnNorth,
--      tiltCycle,
--      doPart2
    ) where

import Data.Char (ord)
import Data.List.Split (splitOn)

import Lib (strip)

doPart1 :: [Char] -> Int
doPart1 input =
  let str = strip input
      parts = splitOn "," str
  in sum $ map (hashFn 0) parts

-- yes, this could be a foldl
hashFn :: Int -> String -> Int
hashFn currVal [] = currVal
hashFn currVal (c:rest) =
  let plusAscii = currVal + ord c
      nextVal = (plusAscii * 17) `mod` 256
  in hashFn nextVal rest
