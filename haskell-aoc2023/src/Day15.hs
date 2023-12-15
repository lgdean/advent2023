module Day15
    (
      doPart1,
      doPart2
    ) where

import Data.Char (ord)
import Data.List (deleteBy)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import Lib (strip)

type Lens = (String, Int)
type Boxes = Map Int [Lens]
data Op = Remove String | Insert Lens deriving Eq

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

doPart2 :: [Char] -> Int
doPart2 input =
  let str = strip input
      parts = splitOn "," str
      ops = map parseOp parts
      boxes = Map.fromAscList $ zip [0..255] $ repeat [] :: Boxes
      endState = foldl operateOn boxes ops :: Boxes
      powers = map (uncurry focusingPower) $ Map.toAscList endState
  in sum powers

operateOn :: Boxes -> Op -> Boxes
operateOn boxes (Remove label) =
  Map.adjust (removeLens label) (hashFn 0 label) boxes
operateOn boxes (Insert lens) =
  Map.adjust (insertOrReplaceLens lens) (hashFn 0 $ fst lens) boxes

removeLens :: String -> [Lens] -> [Lens]
removeLens label = deleteBy (\(a,_) (c,_) -> a == c) (label, 0)

insertOrReplaceLens :: Lens -> [Lens] -> [Lens]
insertOrReplaceLens lens [] = [lens]
insertOrReplaceLens lens@(newL, _) (old@(label, _):rest) =
  if newL == label
  then lens : rest
  else old : insertOrReplaceLens lens rest

focusingPower :: Int -> [Lens] -> Int
focusingPower boxNum lenses =
  (boxNum + 1) * sum (zipWith (*) [1..] (map snd lenses))

parseOp :: String -> Op
parseOp op
  | '=' `elem` op =
      let parts = splitOn "=" op
      in Insert (head parts, read $ last parts)
  | otherwise =
      let parts = splitOn "-" op
      in Remove (head parts)
