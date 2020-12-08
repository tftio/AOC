{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Day07 where

import Text.Regex.TDFA
import Text.RawString.QQ
import Data.List.Split (splitOn)
import qualified Data.HashMap as H

testStr :: String
testStr = "light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags."

type AllData = H.Map String (H.Map String Int)
testData :: AllData
testData = parseInput testStr

parseInput :: String -> AllData
parseInput = H.fromList . map extractMatches . lines
  where
    extractMatches str =
      (bag, bags)
      where
        (_, _, _, matches) = str =~ stringToBag :: (String, String, String, [String]) 
        stringToBag = [r|^([a-z]+ [a-z]+) bags contain (.*)$|]
        bag  = head matches
        bags = head . map (H.fromList . makeBags) . tail $ matches
        makeBags bs
          | bs /= "no other bags." = map makeBag . splitOn ", " $ bs
        makeBags _                 = []

        makeBag b = (adjective ++ " " ++ colour, read n :: Int)
          where
            [n, adjective, colour] = take 3 . words $ b

part01 :: AllData -> Int
part01 allData = H.foldWithKey (\colour subMap acc -> go colour subMap + acc) 0 allData
  where
    go c m =
      let top = allData H.!c in
        if H.member "shiny gold" m then 1 else foldr (\c' acc -> max acc (go c' top)) 0 (H.keys top)

part02 :: AllData -> Int
part02 bagRules = go "shiny gold" - 1
  where
    go colour =
      let mustContain = bagRules H.! colour
      in if null mustContain then 1 else H.foldWithKey (\c count acc -> count * go c + acc) 1 mustContain


test :: Bool
test = part02 testData == 32

solution :: IO ()      
solution = do
  d <- parseInput <$> readFile "data/Day07.txt"
  print (part01 d)
  print (part02 d)

  
  
