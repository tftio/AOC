module Day16 where

import qualified Data.IntSet as S
import Data.List.Split (splitOn)

testInput :: String
testInput = "class: 1-3 or 5-7\nrow: 6-11 or 33-44\nseat: 13-40 or 45-50\n\nyour ticket:\n7,1,14\n\nnearby tickets:\n7,3,47\n40,4,50\n55,2,20\n38,6,12"

testInput2 :: String
testInput2 = "class: 0-1 or 4-19\nrow: 0-5 or 8-19\nseat: 0-13 or 16-19\n\nyour ticket:\n11,12,13\n\nnearby tickets:\n3,9,18\n15,1,5\n5,14,9"

type Ticket = S.IntSet
data Rule = Rule String S.IntSet deriving (Eq, Show)

-- this is so horrible
parseInput :: String -> ([Rule], Ticket, [Ticket])
parseInput str =
  (map lineToRule . lines $ ruleStr, lineToTicket . head . tail . lines $ myTicketStr, map lineToTicket . tail . lines $ ticketsStr)
  where
    [ruleStr,myTicketStr,ticketsStr] = splitOn "\n\n" str
    lineToTicket = S.fromList . map read . splitOn "," 
    lineToRule line =
      Rule name $ pairsToSet pairs
      where
        (name:def) = splitOn ":" line
        pairs = map (map (\s -> read s ::Int) . splitOn "-") . splitOn " or " . tail . head $ def

    pairsToSet = foldr (S.union . pairToSet) S.empty
      where
        pairToSet l = S.fromList [s..e]
          where
            s = head l
            e = head $ tail l

solve01 :: [Ticket] -> [Rule] -> [Int]
solve01 ts rs =
  foldr (\t acc -> acc ++ (S.toList $ findInvalidNumbers t rs)) [] ts

findInvalidNumbers :: Ticket -> [Rule] -> Ticket
findInvalidNumbers =
  foldr removeMatchingNumbers
  where
    removeMatchingNumbers _ ticket | S.null ticket = S.empty
    removeMatchingNumbers (Rule _ ints) ticket     = ticket `S.difference` ints

findValidTickets :: [Ticket] -> [Rule]-> [Ticket]
findValidTickets ts rs =
  filter (\t -> S.null (findInvalidNumbers t rs)) ts


solution = do
  (rules, myTicket, tickets) <- parseInput <$> readFile "data/Day16.txt"
  return . sum $ solve01 tickets rules 
