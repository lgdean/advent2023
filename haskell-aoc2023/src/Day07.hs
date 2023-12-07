module Day07
    (
      doPart1,
--      doPart2
    ) where

import Data.List (group, sort)

data Card = Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 | T | J | Q | K | A deriving (Enum, Eq, Ord, Show)
type Hand = [Card]
data HandType = HighCard | OnePair | TwoPair | ThreeK | FullHouse | FourK | FiveK deriving (Enum, Eq, Ord, Show)

doPart1 :: [Char] -> Int
doPart1 input =
  let rows = map parseLine $ lines input
      sortableHand (hand, bid) = (handType hand, hand, bid)
      sortableHands = map sortableHand rows :: [(HandType, Hand, Int)]
      bestHandsLast = sort sortableHands
      results = zipWith (\rank (_,_,bid) -> rank*bid) [1..] bestHandsLast
  in sum results

--compareSameType :: Hand -> Hand -> Ordering
--compareSameType

handType :: Hand -> HandType
handType hand =
  let samesies = group $ sort hand
  in case length samesies of
    1 -> FiveK
    2 -> if any ((==3) . length) samesies then FullHouse else FourK
    3 -> if any ((==3) . length) samesies then ThreeK else TwoPair
    4 -> OnePair
    _ -> HighCard -- TODO is a hand with a higher high card better? they didn't say

parseLine :: String -> (Hand, Int)
parseLine line =
  let parts = words line
      hand = map parseCard $ head parts
      bid = read $ last parts
  in (hand, bid)

parseCard :: Char -> Card
parseCard '2' = Num2
parseCard '3' = Num3
parseCard '4' = Num4
parseCard '5' = Num5
parseCard '6' = Num6
parseCard '7' = Num7
parseCard '8' = Num8
parseCard '9' = Num9
parseCard 'T' = T
parseCard 'J' = J
parseCard 'Q' = Q
parseCard 'K' = K
parseCard 'A' = A
parseCard  c  = error ("unknown card: " ++ [c])
