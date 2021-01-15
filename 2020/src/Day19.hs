{-# LANGUAGE QuasiQuotes #-}

module Day19 where

import Data.Char (digitToInt, isDigit)
import qualified Data.IntMap as M
import Data.List (find, partition)
import Data.List.Split (splitOn)
import Text.Regex.TDFA ((=~))

testInput :: String
testInput = "0: 4 1 5\n1: 2 3 | 3 2\n2: 4 4 | 5 5\n3: 4 5 | 5 4\n4: \"a\"\n5: \"b\"\n\nababbb\nbababa\nabbbab\naaabbb\naaaabbb"

testInput2 :: String
testInput2 = "0: 1 2\n1: \"a\"\n2: 1 3 | 3 1\n3: \"b\"\n\naaa\naba"

data Rule = A | B | And [Rule] | Or Rule Rule | Singleton Int deriving (Eq, Show)

type Rules = M.IntMap Rule

parseInput :: String -> (Rules, [String])
parseInput str = (M.fromList . map lineToRule . head $ s, head . tail $ s)
  where
    s = map lines . splitOn "\n\n" $ str
    lineToRule :: String -> (Int, Rule)
    lineToRule = p . words
      where
        p (num : rest) =
          (read (take (length num - 1) num) :: Int, parseLine rest)

    parseLine :: [String] -> Rule
    parseLine ["\"a\""] = A
    parseLine ["\"b\""] = B
    parseLine [a] = And [Singleton (read a :: Int)]
    parseLine rs | "|" `elem` rs = Or (parseLine a) (parseLine b)
      where
        [a, b] = splitOn ["|"] rs
    parseLine rs = And $ map (\s -> Singleton (read s :: Int)) rs

toStr :: Rule -> String
toStr r =
  go [] [r]
  where
    go acc [] = acc
    go acc (r : rs) =
      case r of
        A -> go (acc ++ "a") rs
        B -> go (acc ++ "b") rs
        And rs' -> go (go acc rs') rs
        Or a b -> go (acc ++ "(" ++ "(" ++ toStr a ++ ")" ++ "|" ++ "(" ++ toStr b ++ ")" ++ ")") rs

updateFor2 :: Rules -> Rules
updateFor2 =
  M.union
    ( M.fromList
        [ (8, Or (Singleton 42) (And [Singleton 32, Singleton 8])),
          (11, Or (And [Singleton 42, Singleton 31]) (And [Singleton 42, Singleton 11, Singleton 31]))
        ]
    )

rewrite :: Rules -> Rule
rewrite rules = go zero
  where
    zero = (M.!) rules 0

    go B = B
    go A = A
    go (And rs) = And $ map go rs
    go (Singleton i) = go ((M.!) rules i)
    go (Or i j) = Or (go i) (go j)

toRegex rs = "^" ++ s ++ "$"
  where
    s = toStr . rewrite $ rs

go = do
  (rs, msgs) <- parseInput <$> readFile "data/Day19.txt"
  print (toRegex rs)
  print (toRegex . updateFor2 $ rs)

a = And [A, Or (And [Or (And [A, A]) (And [B, B]), Or (And [A, B]) (And [B, A])]) (And [Or (And [A, B]) (And [B, A]), Or (And [A, A]) (And [B, B])]), B]
