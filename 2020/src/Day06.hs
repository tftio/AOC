module Day06 where
import Data.List.Split (splitOn)
import qualified Data.Set as S 

testData :: String
testData = "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"

parseData :: [Char] -> [[S.Set Char]]
parseData = map (map S.fromList . lines) . splitOn "\n\n"

solve :: ([S.Set Char] -> S.Set Char) -> Int
solve f = sum . map (S.size . f) 

part01 :: [S.Set Char] -> S.Set Char 
part01 = foldr S.union S.empty

part02 :: [S.Set Char] -> S.Set Char
part02 []     = S.empty
part02 (s:ss) = foldr S.intersection s ss

solution :: IO ()
solution = do
  s <- parseData <$> readFile "data/Day06.txt"
  print (solve part01 s)
  print (solve part02 s)
