{-# LANGUAGE TupleSections #-}
module Day20
    (
      doPart1,
      doPart2
    ) where

import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)

import Debug.Trace (trace)

import Lib (count)

data Pulse = Low | High deriving (Eq, Show)
type Message = (String, String, Pulse)

data FlipFlopState = On | Off deriving (Eq, Show)
data Module = FlipFlop FlipFlopState | Conjunction (Map String Pulse) | Broadcast deriving (Eq, Show)
type ModuleStates = Map String Module   -- what modules do we have?
type ModuleConfig = Map String [String] -- and how are they hooked up?
initFlipFlop :: Module
initFlipFlop = FlipFlop Off
initConjunction :: [String] -> Module
initConjunction inputs = Conjunction (Map.fromList $ zip inputs $ repeat Low)

doPart1 :: [Char] -> Int
doPart1 input =
  let (moduleConfig, modulePreInitStates) = parseConfig input
      moduleInitStates = initializeConjunctions moduleConfig modulePreInitStates
      countPulses (_, ps) = (count Low ps, count High ps)
      results = iterate (\(s, _) -> pushButton moduleConfig s) (moduleInitStates, [])
      totals = take 1000 $ tail $ map countPulses results
  in sum (map fst totals) * sum (map snd totals)

pushButton :: ModuleConfig -> ModuleStates -> (ModuleStates, [Pulse])
pushButton config moduleStates =
  let (newState, result) = propagate config moduleStates [("button", "broadcaster", Low)]
  in (newState, map (\(_, _, p) -> p) result)

propagate :: ModuleConfig -> ModuleStates -> [(String, String, Pulse)] -> (ModuleStates, [Message])
propagate config modules messages =
  let fullRound = scanl (\(acc, _) m -> propagateOne config acc m) (modules, []) messages :: [(ModuleStates, [Message])]
      nextRound = concatMap snd fullRound :: [Message]
      nextRoundState = fst $ last fullRound :: ModuleStates
  in case nextRound of
       [] -> (nextRoundState, messages)
       _ -> (\(s, ms) -> (s, messages ++ ms)) $ propagate config nextRoundState nextRound

propagateOne :: ModuleConfig -> ModuleStates -> (String, String, Pulse) -> (ModuleStates, [(String, String, Pulse)])
propagateOne _      modules _msg@(_  , dest, _    ) | dest `Map.notMember` modules = (modules, [])
propagateOne config modules _msg@(src, dest, pulse) =
  let srcMod = modules Map.! dest
      _result@(newState, nextP) = sendPulse (pulse, src) srcMod
      updatedModules = Map.insert dest newState modules
  in case nextP of
       Nothing -> (updatedModules, [])
       Just nP ->
         let nextDests = config Map.! dest
         in (updatedModules, map (dest, , nP) nextDests)

sendPulse :: (Pulse, String) -> Module -> (Module, Maybe Pulse)
sendPulse (High, _) ff@(FlipFlop _) = (ff, Nothing)
sendPulse (Low, _) (FlipFlop Off) = (FlipFlop On, Just High)
sendPulse (Low, _) (FlipFlop On) = (FlipFlop Off, Just Low)
sendPulse (p, src) (Conjunction prev) =
  let newStates = Map.insert src p prev
      allHigh = all (== High) $ Map.elems newStates
  in (Conjunction newStates, Just (if allHigh then Low else High))
sendPulse (p, _) Broadcast = (Broadcast, Just p)

initializeConjunctions :: ModuleConfig -> ModuleStates -> ModuleStates
initializeConjunctions config initStates =
  let conjunctions = Map.keys $ Map.filter (== Conjunction Map.empty) initStates
      sources conjName = Map.keys $ Map.filter (conjName `elem`) config
      newConjunctions = map (\name -> (name, initConjunction $ sources name)) conjunctions
  in foldl (\acc (k,v) -> Map.insert k v acc) initStates newConjunctions

parseConfig :: String -> (ModuleConfig, ModuleStates)
parseConfig str =
  let moduleInfos = map parseModuleLine $ lines str
      config = Map.fromList $ map (\(name, _, dests) -> (name, dests)) moduleInfos
      preInitStates = Map.fromList $ map (\(name, m, _) -> (name, m)) moduleInfos
  in (config, preInitStates)

parseModuleLine :: String -> (String, Module, [String])
parseModuleLine line =
  let parts = splitOn " -> " line
      modulePart = head parts
      destPart = last parts
      (name, m) = parseModule modulePart
  in (name, m, splitOn ", " destPart)

parseModule :: String -> (String, Module)
parseModule "broadcaster" = ("broadcaster", Broadcast)
parseModule ('%':name)  = (name, initFlipFlop)
parseModule ('&':name)  = (name, initConjunction []) -- oops, don't yet know inputs
parseModule otherString = error ("cannot parse Module: " ++ otherString)


doPart2 :: [Char] -> Int
doPart2 input =
  let (moduleConfig, modulePreInitStates) = parseConfig input
      moduleInitStates = initializeConjunctions moduleConfig modulePreInitStates
      results = iterate (\(s, _) -> pushButton2 moduleConfig s) (moduleInitStates, [])
      -- $ grep rx inputs/day20
      -- &qb -> rx
      highMessageToQb (src, dest, p) = if p == High && dest == "qb" then Just (src, p) else Nothing
      maybeHighMessage (_, msgs) = mapMaybe highMessageToQb msgs :: [(String, Pulse)]
      highsToQb = zip [0..] (map maybeHighMessage results) :: [(Int, [(String, Pulse)])]
      -- qb takes 4 inputs, and maybe there is some cycling happening...
      firstFourHighs = take 4 $ filter (not . null . snd) highsToQb
      -- I just visually inspected that they were for 4 different inputs
  in trace (show firstFourHighs) $ foldl lcm 1 $ map fst firstFourHighs

pushButton2 :: ModuleConfig -> ModuleStates -> (ModuleStates, [Message])
pushButton2 config moduleStates =
  let (newState, result) = propagate config moduleStates [("button", "broadcaster", Low)]
  in (newState, result)
