module Day07
    (
      doPart1,
      doPart2
    ) where

import Data.List (group, sort, sortBy)

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

doPart2 :: [Char] -> Int
doPart2 input =
  let rows = map parseLine $ lines input
      --sortableHand (hand, bid) = (part2HandType hand, hand, bid)
      bestHandsLast = sortBy compareForPart2 rows
      results = zipWith (\rank (_,bid) -> rank*bid) [1..] bestHandsLast
  in sum results

-- there is surely a way to combine some of these functions with less boilerplate
compareForPart2 :: (Hand, Int) -> (Hand, Int) -> Ordering
compareForPart2 (h1, _bid1) (h2, _bid2) =
  compareHandsForPart2 h1 h2

compareHandsForPart2 :: Hand -> Hand -> Ordering
compareHandsForPart2 h1 h2 =
  case compare (part2HandType h1) (part2HandType h2) of
    EQ -> compareSameTypeHandsForPart2 h1 h2
    _  -> compare (part2HandType h1) (part2HandType h2)

compareSameTypeHandsForPart2 :: Hand -> Hand -> Ordering
compareSameTypeHandsForPart2 [] [] = EQ
compareSameTypeHandsForPart2 (x:xs) (y:ys) =
  case compareCardsForPart2 x y of
    EQ -> compareSameTypeHandsForPart2 xs ys
    LT -> LT
    GT -> GT
compareSameTypeHandsForPart2 _ _ = error "tried to compare hands of different size"

compareCardsForPart2 :: Card -> Card -> Ordering
compareCardsForPart2 J J = EQ
compareCardsForPart2 J _ = LT
compareCardsForPart2 _ J = GT
compareCardsForPart2 c1 c2 = compare c1 c2

-- not pretty, but highly effective at solving this part of the puzzle
part2HandType :: Hand -> HandType
part2HandType hand | J `notElem` hand = handType hand -- use logic from part 1
part2HandType hand | count J hand == 1 =
  let samesies = group $ sort $ filter (/= J) hand
  in case length samesies of
    1 -> FiveK
    2 -> if any ((==3) . length) samesies then FourK else FullHouse -- opposite of Part 1!
    3 -> ThreeK
    4 -> OnePair
    _ -> error "should not reach"
part2HandType hand | count J hand == 2 =
  let samesies = group $ sort $ filter (/= J) hand
  in case length samesies of
    1 -> FiveK
    2 -> FourK
    3 -> ThreeK
    _ -> error "should not reach"
part2HandType hand | count J hand == 3 =
  let samesies = group $ sort $ filter (/= J) hand
  in case length samesies of
    1 -> FiveK
    2 -> FourK
    _ -> error "should not reach"
part2HandType _ = FiveK

count :: Eq a => a -> [a] -> Int
count x xs = length $ filter (== x) xs
