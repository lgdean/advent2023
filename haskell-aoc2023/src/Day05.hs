module Day05
    (
      doPart1,
      doPart2
    ) where

import Lib (parseChunks)

import Data.List (sort)
import Data.List.Split (chunksOf)

-- srcStart, destStart, rangeLength
type MappingRange = (Int, Int, Int)
type Mapping = [MappingRange] -- sorted on srcStart, for now

doPart1 :: [Char] -> Int
doPart1 input =
  let mappings = parseMappings input
      seeds = parseSeedList $ head $ lines input
      mapSeed seed = foldl processMapping seed mappings
      destLocations = map mapSeed seeds
  in minimum destLocations

processMapping :: Int -> Mapping -> Int
processMapping src [] = src
processMapping src ((srcStart, destStart, rangeLength):_rest)
  | srcStart <= src && src < (srcStart+rangeLength) = (src - srcStart) + destStart
processMapping  src(_:rest) = processMapping src rest

-- takes the whole input file, including the first line (irrelevant seed list)
parseMappings :: String -> [Mapping]
parseMappings input =
  tail $ parseChunks parseMapping input

parseMapping :: [String] -> Mapping
parseMapping linesAndHeader =
  let mappingLines = tail linesAndHeader
  in sort $ map parseMappingRange mappingLines

parseMappingRange :: String -> MappingRange
parseMappingRange line =
  let parts = words line
  in case parts of
       [d, s, l] -> (read s, read d, read l)
       _ -> error ("unable to parse range: " ++ show parts)

parseSeedList :: String -> [Int]
parseSeedList line = map read $ tail $ words line

doPart2 :: [Char] -> Int
doPart2 input =
  let mappings = parseMappings input
      seedPairs = parseSeedRanges $ head $ lines input
      seeds = concatMap explodeRange seedPairs -- start with brute force
      mapSeed seed = foldl processMapping seed mappings
      destLocations = map mapSeed seeds
  in minimum destLocations

parseSeedRanges :: String -> [(Int, Int)]
parseSeedRanges line =
  let parts = chunksOf 2 $ tail $ words line
      mapPair p = (read (head p), read (last p))
  in map mapPair parts

explodeRange :: (Int, Int) -> [Int]
explodeRange (start, len) = take len [start ..]
