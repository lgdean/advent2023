module Day06
    (
      doPart1,
--      doPart2
    ) where

-- This isn't actually how I solved it in the moment.
-- It's less error-prone than what I actually did,
-- but involved more frustration with inscrutable type errors.
-- float, integral, double -- keywords for my future self

doPart1 :: [(Int, Int)] -> Int
doPart1 input =
  let results = map howManySolutions input
  in product results

howManySolutions :: (Int, Int) -> Int
howManySolutions (time, record) =
  let lower = lowerSolution (time, record)
  in time + 1 - 2 * lower

lowerSolution :: (Int, Int) -> Int
lowerSolution (time, record) =
  let betterThanRecord = record + 1
      partToRoot = time * time - 4 * betterThanRecord
      root = sqrt (fromIntegral partToRoot)
      result = (fromIntegral time - root) / 2 :: Double
  in ceiling result
