module Day01 where

import Data.Char (isDigit)

doPart1 :: [Char] -> Int
doPart1 input =
  let strings = lines input
      digits = map (filter isDigit) strings
      nums = map numberFrom digits
  in sum nums

numberFrom :: [Char] -> Int
numberFrom [x,y] = read [x,y]
numberFrom [x] = read [x,x]
numberFrom (x:rest) = read [x, last rest]
numberFrom [] = 0
