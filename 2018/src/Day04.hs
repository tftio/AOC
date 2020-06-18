{-# LANGUAGE FlexibleContexts #-}
module Day04 where

import Data.List
import qualified Data.Map as Map
import Data.List.Split

type Minute = Int
type AsleepRecord = Map.Map Minute Int

data Guard      = Guard Int       | Previous                            deriving Show
data LineState  = NewGuard Minute | GoesToSleep Minute | WakesUp Minute deriving Show

pad a b =  (map (\_ -> 0) [0 .. (a - 1)])  ++
           (map (\_ -> 1) [a .. b])        ++
           (map (\_ -> 0) [(b + 1) .. 59])

extractAsleepRange :: LineState -> LineState -> [Int]
extractAsleepRange s s' = case (s, s') of
    (GoesToSleep m, WakesUp  m') -> pad m (m' - 1)
    (GoesToSleep m, NewGuard m') -> pad m (m' - 1)
    (_, _)                       -> map (\_ -> 0) [0..59]
    
addAsleepRange :: Int -> [Int] -> AsleepRecord -> AsleepRecord
addAsleepRange g ms acc = Map.unionWith (+) acc (Map.fromList [(m, 1) | m <- ms])

combineAsleepRanges :: [Int] -> [Int] -> [Int]
combineAsleepRanges a b = map (\(a, b) -> a + b) (zip a b)

parseLine :: [Char] -> (Guard, LineState)
parseLine l = case (splitLine l) of
  mm:"Guard":num:_ -> (Guard (read num :: Int), NewGuard    (read mm :: Int))
  mm:"falls":_     -> (Previous,                GoesToSleep (read mm :: Int))
  mm:"wakes":_     -> (Previous,                WakesUp     (read mm :: Int))
  where
    splitLine = drop 4 . split (condense . dropDelims . dropInnerBlanks . dropInitBlank . dropFinalBlank $ oneOf "[]- :#")

extractSleepRanges :: [(Guard, LineState)] -> Map.Map Int [Int]
extractSleepRanges gs = esr 0 Map.empty (zip gs (tail gs)) where
  esr currentGuard acc []       = acc
  esr currentGuard acc (((g, s), (_, s')):ls) = esr newGuard newAcc ls where
    newGuard = case g of Guard i -> i
                         Previous -> currentGuard
    newAcc   = Map.unionWith (\l l' -> map (\(a, b) -> a + b) (zip l l')) acc (Map.singleton newGuard (extractAsleepRange s s'))

toSleepyMap :: Map.Map Int [Int] -> Map.Map Int Int
toSleepyMap m = Map.mapMaybe (\ms -> Just (maximum ms)) m

getSleepiestGuard :: Map.Map Int [Int] -> Int
getSleepiestGuard = fst . head . reverse . (sortOn snd) . Map.toList . toSleepyMap

getMaxMinuteForGuard :: Int -> Map.Map Int [Int] -> Int
getMaxMinuteForGuard g allMins =  fst (foldl (\(m, v) (m', v') -> if (v > v') then (m, v) else (m', v')) (head mins) (tail mins)) where
    mins = zip [0..59] (allMins Map.! g)

createMap = extractSleepRanges . map parseLine . sort . lines

solve01 m = min * guard where
  guard = getMaxGuard m
  min   = getMaxMinuteForGuard guard m
  getMaxGuard = fst . head . (sortBy (\(_, v) (_, v') -> compare v' v)) . Map.toList . (Map.mapMaybe (\is -> Just (foldl (+) 0 is)))


solve02 m = min * guard where
  guard = getSleepiestGuard m
  min   = getMaxMinuteForGuard guard m
  
testData = "[1518-11-01 00:00] Guard #10 begins shift\n[1518-11-01 00:05] falls asleep\n[1518-11-01 00:25] wakes up\n[1518-11-01 00:30] falls asleep\n[1518-11-01 00:55] wakes up\n[1518-11-01 23:58] Guard #99 begins shift\n[1518-11-02 00:40] falls asleep\n[1518-11-02 00:50] wakes up\n[1518-11-03 00:05] Guard #10 begins shift\n[1518-11-03 00:24] falls asleep\n[1518-11-03 00:29] wakes up\n[1518-11-04 00:02] Guard #99 begins shift\n[1518-11-04 00:36] falls asleep\n[1518-11-04 00:46] wakes up\n[1518-11-05 00:03] Guard #99 begins shift\n[1518-11-05 00:45] falls asleep\n[1518-11-05 00:55] wakes up"

solution01 = do
  s <- readFile "data/Day04.txt"
  print (solve01 (createMap s))

solution02 = do
  s <- readFile "data/Day04.txt"
  print(solve02 (createMap s))
