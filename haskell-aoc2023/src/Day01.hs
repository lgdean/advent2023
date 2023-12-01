module Day01
    (
      doPart1,
      doPart2
    ) where

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

doPart2 :: [Char] -> Int
doPart2 input =
  let strings = lines input
      nums = map fancyNumberFrom strings
  in sum nums

fancyNumberFrom :: [Char] -> Int
fancyNumberFrom chars =
  let digits = allDigitsFrom chars
  in (10 * head digits) + last digits

allDigitsFrom :: [Char] -> [Int]
allDigitsFrom [] = []
allDigitsFrom (x:rest) | isDigit x = (read [x] :: Int) : allDigitsFrom rest
-- there are better ways, but this will do
allDigitsFrom ('z':'e':'r':'o':rest) = 0 : allDigitsFrom ('o':rest)
allDigitsFrom ('o':'n':'e':rest) = 1 : allDigitsFrom ('e':rest)
allDigitsFrom ('t':'w':'o':rest) = 2 : allDigitsFrom ('o':rest)
allDigitsFrom ('t':'h':'r':'e':'e':rest) = 3 : allDigitsFrom ('e':rest)
allDigitsFrom ('f':'o':'u':'r':rest) = 4 : allDigitsFrom rest
allDigitsFrom ('f':'i':'v':'e':rest) = 5 : allDigitsFrom ('e':rest)
allDigitsFrom ('s':'i':'x':rest) = 6 : allDigitsFrom rest
allDigitsFrom ('s':'e':'v':'e':'n':rest) = 7 : allDigitsFrom rest
allDigitsFrom ('e':'i':'g':'h':'t':rest) = 8 : allDigitsFrom ('t':rest)
allDigitsFrom ('n':'i':'n':'e':rest) = 9 : allDigitsFrom ('e':rest)
allDigitsFrom (_:rest) = allDigitsFrom rest
