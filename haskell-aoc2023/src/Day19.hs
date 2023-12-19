module Day19
    (
      doPart1,
--      doPart2
    ) where

import Data.Char (isAlpha, isDigit)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)

data PartRating = PartRating
    { x :: Int
    , m :: Int
    , a :: Int
    , s :: Int
    } deriving (Show)
data Op = LessThan | GreaterThan deriving (Eq, Show)
type WorkflowRule = (PartRating -> Int, Op, Int, String)
type Workflows = Map String [WorkflowRule]
data Result = Accept | Reject deriving (Eq, Show)

doPart1 :: [Char] -> Int
doPart1 input =
  let allLines = lines input
      chunks = splitOn [""] allLines
      workflowLines = head chunks
      partRatings = map parseRating $ last chunks
      workflowMap = Map.fromList $ map parseWorkflow workflowLines :: Workflows
      partResults = map (doWorkflow workflowMap "in") partRatings
      acceptedParts = map fst $ filter ((== Accept) . snd) $ zip partRatings partResults
      partSum p = x p + m p + a p + s p
  in sum $ map partSum acceptedParts

doWorkflow :: Workflows -> String -> PartRating -> Result
doWorkflow _ "A" _ = Accept
doWorkflow _ "R" _ = Reject
doWorkflow workflows workflowName part =
  let flow = workflows Map.! workflowName
      nextState = flowThrough flow part
  in doWorkflow workflows nextState part

flowThrough :: [WorkflowRule] -> PartRating -> String
flowThrough rules part = head $ mapMaybe (`applyRule` part) rules

applyRule :: WorkflowRule -> PartRating -> Maybe String
applyRule (whichRating, LessThan, threshold, result) part =
  if whichRating part < threshold then Just result else Nothing
applyRule (whichRating, GreaterThan, threshold, result) part =
  if whichRating part > threshold then Just result else Nothing

parseWorkflow :: String -> (String, [WorkflowRule])
parseWorkflow line =
  let parts = splitOn "{" line
      name = head parts
      ruleList = splitOn "," $ filter (/= '}') $ last parts
  in (name, map parseWorkflowRule ruleList)

parseWorkflowRule :: String -> WorkflowRule
parseWorkflowRule r | all isAlpha r = (x, GreaterThan, -1, r) -- a hack
parseWorkflowRule (r:o:rest) =
  let parts = splitOn ":" rest
  in (parseWhichRating r, parseOp o, read (head parts), last parts)
parseWorkflowRule r = error ("could not parse rule: " ++ r)

parseWhichRating :: Char -> (PartRating -> Int)
parseWhichRating 'x' = x
parseWhichRating 'm' = m
parseWhichRating 'a' = a
parseWhichRating 's' = s
parseWhichRating  c  = error ("unknown rating type: " ++ [c])

parseOp :: Char -> Op
parseOp '<' = LessThan
parseOp '>' = GreaterThan
parseOp  c  = error ("unknown comparison op: " ++ [c])

-- there are json parsing libraries in the world...
parseRating :: String -> PartRating
parseRating str =
  let parts = splitOn "," $ filter (`notElem` "{}") str
      readVal p = read $ filter isDigit p
      vals = map readVal parts
  in PartRating { x = head vals, m = vals !! 1, a = vals !! 2, s = vals !! 3 }
