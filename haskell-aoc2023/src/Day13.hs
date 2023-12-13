module Day13
    (
      doPart1,
      doPart2
    ) where

import Data.List (isPrefixOf, transpose)

import Debug.Trace (trace)

import Lib (parseChunks, strip)

doPart1 :: [Char] -> Int
doPart1 input =
  let patterns = parseChunks (map strip) input :: [[[Char]]]
      rowColNums = map findMirrors patterns
  in sum $ map summaryFrom rowColNums

summaryFrom :: Num a => (Maybe a, Maybe a) -> a
summaryFrom (Just r, Nothing) = 100*r
summaryFrom (Nothing, Just c) = c
summaryFrom (Nothing, Nothing) = error "no vertical or horizontal mirror found"
summaryFrom (Just _, Just _) = error "do some of them have both?"

findMirrors :: [[Char]] -> (Maybe Int, Maybe Int)
findMirrors pattern =
  (findRowMirror pattern, findRowMirror $ transpose pattern)

findRowMirror :: [[Char]] -> Maybe Int
findRowMirror rows =
  let allCombos = zipWith splitAt [1.. (length rows - 1)] (repeat rows) :: [([[Char]], [[Char]])]
      isMirrored (before, after) =
        (reverse before) `isPrefixOf` after || after `isPrefixOf` (reverse before)
      results = filter isMirrored allCombos
  in if null results then Nothing else Just $ (length . fst . head) results

doPart2 :: [Char] -> Int
doPart2 input =
  let patterns = parseChunks (map strip) input :: [[[Char]]]
      rowColNums = map findSmudgeMirrors patterns
  in sum $ map summaryFrom rowColNums

findSmudgeMirrors :: [[Char]] -> (Maybe Int, Maybe Int)
findSmudgeMirrors pattern =
  (findRowSmudgeMirror pattern, findRowSmudgeMirror $ transpose pattern)

findRowSmudgeMirror :: [[Char]] -> Maybe Int
findRowSmudgeMirror rows =
  let allCombos = zipWith splitAt [1.. (length rows - 1)] (repeat rows) :: [([[Char]], [[Char]])]
      isMirrored (before, after) =
        offByJustOne (reverse before) after
      results = filter isMirrored allCombos
  in if null results then Nothing else Just $ (length . fst . head) results

offByJustOne :: Eq a => [[a]] -> [[a]] -> Bool
offByJustOne xs ys | length xs > length ys = someFunction ys xs
offByJustOne xs ys = someFunction xs ys

someFunction :: Eq a => [[a]] -> [[a]] -> Bool
someFunction [] _ = False
someFunction _ [] = error "called with first longer than second, no good"
someFunction (xs:xss) (ys:yss) | xs == ys = someFunction xss yss
someFunction (xs:xss) (ys:yss) =
  case length $ filter (uncurry (/=)) $ zip xs ys of
    0 -> trace "thought I had already covered this case above" $ someFunction xss yss
    1 -> xss `isPrefixOf` yss
    _ -> False
