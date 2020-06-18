module Day04 where

import qualified Data.Set
import qualified Data.List

stringToSet f = Data.Set.fromList . (map f) . words
totalNumWords = length . words
uniqueNumWords = length . stringToSet (\x -> x)
anagramUniqueWords = length . stringToSet Data.List.sort

validPartOne s = totalNumWords s == uniqueNumWords s
validPartTwo s = totalNumWords s == anagramUniqueWords s

main = do
  txt <- readFile "/Users/jamesblack/Projects/AOC-2017/data/Day04.txt"
  let ls = lines txt
  let partOne = length (filter validPartOne ls)
  let partTwo = length (filter validPartTwo ls)
  print (show [partOne, partTwo])
