module Day05 where
import qualified Data.Char as Char
import Data.List
testData = "dabAcCaCBAcCcaDA"

areReactive a b =
  (Char.toUpper a == Char.toUpper b) && ((Char.isUpper a && Char.isLower b) || (Char.isLower a && Char.isUpper b))

reducePolymer :: String -> String
reducePolymer p = (foldr r "" p)
  where
    r x (y:ys) | areReactive x y = ys
    r x ys                       = x : ys

removeUnit u p = filter (\c -> not (Char.toUpper c == Char.toUpper u)) p

solve02 p = Data.List.minimum (map (\u -> length (reducePolymer (removeUnit u p))) ['a' .. 'z'])

solve01 = length . reducePolymer
solution01 = do
  s <- readFile "data/Day05.txt"
  print (solve01 s)

solution02 = do
  s <- readFile "data/Day05.txt"
  print (solve02 s)

