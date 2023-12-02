module Day02
    (
      doPart1,
      doPart2
    ) where

import Data.List.Split(splitOn)
import Data.Map (findWithDefault)
import qualified Data.Map.Strict as Map

doPart1 :: [Char] -> Int
doPart1 input =
  let allGames = map parseLine $ lines input
      isPossible game = isAllowedFrom (12,13,14) $ snd game
  in sum $ map fst $ filter isPossible allGames

isAllowedFrom :: (Int, Int, Int) -> [(Int, Int, Int)] -> Bool
isAllowedFrom _ [] = True
isAllowedFrom (rTotal, gTotal, bTotal) ((r,g,b):_) | rTotal < r || gTotal < g || bTotal < b = False
isAllowedFrom (rTotal, gTotal, bTotal) (_:rest) =
  let tooBig (r,g,b) = rTotal < r || gTotal < g || bTotal < b
  in not $ any tooBig rest

doPart2 :: [Char] -> Int
doPart2 input =
  let allGames = map parseLine $ lines input
      minimalSet game = foldl (\(a,b,c) (x,y,z) -> (max a x, max b y, max c z)) (0,0,0) $ snd game
  in sum $ map (\(x,y,z) -> x*y*z) $ map minimalSet allGames

-- use order R,G,B
parseLine :: String -> (Int, [(Int, Int, Int)])
parseLine line =
  let [header, rest] = splitOn ":" line
      gameId = read $ head $ tail $ words header
      handfuls = splitOn ";" rest
  in (gameId, map parseHandful handfuls)

parseHandful :: String -> (Int, Int, Int)
parseHandful handful =
  let parts = splitOn "," handful
      colors = map parseColor parts
      colorMap = Map.fromList colors
  in (findWithDefault 0 "red" colorMap,
      findWithDefault 0 "green" colorMap,
      findWithDefault 0 "blue" colorMap)

parseColor :: String -> (String, Int)
parseColor color =
  let parts = words color
  in (last parts, read $ head parts)
