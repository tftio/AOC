{-# LANGUAGE OverloadedStrings #-}

module Day07 where

import Data.List
import qualified Data.Text as Text

type Part = Text.Text
type Address = ([Part], [Part])

testData :: [(Text.Text, Bool)]
testData = [("abba[mnop]qrst", True),
            ("abcd[bddb]xyyx", False),
            ("aaaa[qwer]tyui", False),
            ("ioxxoj[asdfgh]zxcvbn", True)]

containsPalindrome = any isLegalPalindrome . (partitionStr 4)
isLegalPalindrome t
  | not (Text.length t == 4) = False
  | otherwise =
      let (a:b:c:d:_) = Text.chunksOf 1 t in
        a == d && b == c && not (a == b)

legalTriples :: Text.Text -> [Text.Text]
legalTriples = filter isLegalTriple . (partitionStr 3)

invertTriple :: Text.Text -> Text.Text
invertTriple input =
  let a:b:_ = (Text.chunksOf 1 input) in
    Text.concat [b,a,b]

isLegalTriple t
  | not (Text.length t == 3) = False
  | otherwise =
    let (a:b:c:_) = Text.chunksOf 1 t in
      a == c && not (b == a)

partitionStr :: Int -> Text.Text -> [Text.Text]
partitionStr len input =
  reverse (partition [] (Text.chunksOf 1 input))
  where
    partition acc [] = acc
    partition acc ls
      | length ls < len = acc
      | otherwise =
        let a = Text.concat (take len ls)
            rest = tail ls
        in
          partition (a:acc) rest

isLegalPartOneAddress :: Address -> Bool
isLegalPartOneAddress (i, o) =
  all (\a -> not (containsPalindrome a)) i && any containsPalindrome o

isLegalPartTwoAddress :: Address -> Bool
isLegalPartTwoAddress (outs, ins) =
  let allOutTriples = concat (map legalTriples outs) in
  check False allOutTriples ins
  where
    contains triple str = Text.count (invertTriple triple) str > 0 && Text.count triple str == 0
    check True _ _  = True
    check seen _ [] = seen
    check seen [] _ = seen
    check seen (t:ts) (o:os) =
      seen || (contains t o) || check seen ts os

parseAddress text =
  let parts = Text.split (\c -> c == ']' || c == '[') text
      (outsides, insides) = partition (\(idx, part) -> idx `mod` 2 == 0) (zip [0..] parts)
      removeIdx = map (\(_, p) -> p)
  in
    (removeIdx outsides, removeIdx insides)

partOne = length . filter isLegalPartOneAddress . map parseAddress . Text.lines . Text.pack
partTwo = length . filter isLegalPartTwoAddress . map parseAddress . Text.lines . Text.pack
