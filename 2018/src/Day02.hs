module Day02 where
import           Data.List

hasNLetters :: Int -> [Char] -> Bool
hasNLetters n = elem n . map length . group . sort

checksum :: [[Char]] -> Int
checksum ns = length (filter (\a -> hasNLetters 2 a) ns) * length (filter (\a -> hasNLetters 3 a) ns)

differs :: [Char] -> [Char]-> Bool
differs a b =
  dfs 0 a b where
  dfs 1 [] _            = True
  dfs 1 _ []            = True
  dfs _ [] _            = False
  dfs _ _ []            = False
  dfs acc (a:as) (b:bs) = dfs (acc + if a /= b then 1 else 0) as bs

common :: [Char] -> [Char] -> [Char]
common a b = reverse (cmn [] a b)
  where
    cmn acc [] _          = acc
    cmn acc _ []          = acc
    cmn acc (a:as) (b:bs) = cmn (if a == b then a : acc else acc) as bs

testInput1 = "abcdef bababc abbcde abcccd aabcdd abcdee ababab"
testInput2 = "abcde fghij klmno pqrst fguij axcye wvxyz"

solve1 l = checksum (words l)
solve2 l = [common x y | x <- words l, y <- words l, differs x y]

solution1 = do
  l <- readFile "./data/Day02.txt"
  print (solve1 l)

solution2 = do
  l <- readFile "./data/Day02.txt"
  print (solve2 l)
