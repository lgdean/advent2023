module Day08
    (
      doPart1,
--      doPart2
    ) where

import Data.List.Split(splitOn)
import Data.Map (Map, (!))
import qualified Data.Map.Strict as Map

type NodeMap = Map String (String, String)

doPart1 :: [Char] -> Int
doPart1 input =
  let allLines = lines input
      directions = cycle $ head allLines
      nodes = map parseNode $ drop 2 allLines
      nodeMap = Map.fromList nodes
  in findLengthOfGivenPath directions nodeMap "AAA" "ZZZ"

findLengthOfGivenPath :: String -> NodeMap -> String -> String -> Int
findLengthOfGivenPath directions nodeMap src dest =
  let infinitePath = scanl (goDir nodeMap) src directions :: [String]
  in length $ takeWhile (dest /=) infinitePath

goDir :: NodeMap -> String -> Char -> String
goDir nodeMap src dir =
  let makeTurn = if dir == 'L' then fst else snd
      options = nodeMap ! src
  in makeTurn options

parseNode :: String -> (String, (String, String))
parseNode line =
  let parts = splitOn " = " line
      src = head parts
      (leftDest:rightDest:_) = splitOn "," (parts !! 1)
      nodeName = filter (`notElem` " ()")
  in (src, (nodeName leftDest, nodeName rightDest))
