module Day14 where
import Data.Foldable (toList)
import Data.Char (intToDigit)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Data.List (intercalate, length)
import Debug.Trace (trace)

type Index = Int
type Recipe = Int

data Elf = Elf Index Recipe deriving (Show)

recipes :: [Recipe]
recipes = [3,7]

elfA = Elf 0 3
elfB = Elf 1 7

fetchNextRecipe :: Elf -> [Recipe] -> Elf
fetchNextRecipe (Elf i r) rs =
  Elf i' r'
  where
    i' = (r + i + 1) `mod` (length rs)
    r' = (cycle rs) !! i'

addNewRecipes :: Elf -> Elf -> [Recipe] -> [Recipe]
addNewRecipes (Elf _ a) (Elf _ b) rs = rs ++ digits (a + b)

digits :: Integral a => a -> [a]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

drawBoard :: Elf -> Elf -> [Recipe] -> String
drawBoard (Elf ia _) (Elf ib _) rs =
  intercalate "" rs'
  where
    rs' = map (\(i, v) ->
                 case i of
                   _ | ia == i -> "(" ++ show v ++ ")"
                   _ | ib == i -> "[" ++ show v ++ "]"
                   _           -> " " ++ show v) (zip [0..] rs)
iterateOnce :: Elf -> Elf -> [Recipe] -> (Elf, Elf, [Recipe])
iterateOnce a b rs =
  (a', b', rs')
  where
    rs' = addNewRecipes a b rs
    a'  = fetchNextRecipe a rs'
    b'  = fetchNextRecipe b rs'

iterateN :: Int -> Elf -> Elf -> [Recipe] -> (Elf, Elf, [Recipe])
iterateN 0 a b rs =  (a, b, rs)
iterateN i a b rs = iterateN (i - 1) a' b' rs' 
  where (a', b', rs') = iterateOnce a b rs

solveFor :: Int -> Int -> Elf -> Elf -> [Recipe] -> String
solveFor toDrop toTake a b recipes =
  fmt answer
  where
    fmt = intercalate "" . map show
    answer = trace (fmt finalList) take toTake (drop toDrop finalList)
    
    finalList = aux a b recipes
    aux a b rs =
      if (length rs) >= (toDrop + toTake) then
        rs
      else
        let
          (a', b', rs') = iterateOnce a b rs
        in
          aux a' b' rs'

testSolutions =
  map (\(d, t, a) -> solveFor d t elfA elfB recipes == a)
  [(5, 10, "0124515891"),
   (9, 10, "5158916779"),
   (18, 10, "9251071085"),
   (2018, 10, "5941429882")]

part01 :: Seq Int -> (Int,Int) -> Int -> Int
part01 xs (i,j) n 
      | length xs > (n + 10) = read (map intToDigit $ toList (Seq.take 10 (Seq.drop n xs))) :: Int
      | otherwise            = part01 xs' (i',j') n
      where
        s       = Seq.length xs 
        x       = xs `Seq.index` (i `mod` s)
        y       = xs `Seq.index` (j `mod` s)
        xy      = if x+y >= 10 then Seq.singleton 1 Seq.|> ((x+y) `mod` 10) else Seq.singleton (x+y)
        xs'     = xs Seq.>< xy
        (i',j') = ((i + x + 1) `mod` length xs', (j + y + 1) `mod` length xs')

part02 :: Seq Int -> (Int,Int) -> Seq Int -> Int
part02 xs (i,j) ns
      | ns'' == ns = Seq.length left
      | otherwise  = part02 xs' (i',j') ns
      where
        s          = Seq.length xs 
        (left,ns') = Seq.splitAt (s - Seq.length ns - 2) xs
        ns''       = Seq.take (Seq.length ns) ns'
        x          = xs `Seq.index` (i `mod` s)
        y          = xs `Seq.index` (j `mod` s)
        xy         = x + y
        xs'        = if xy >= 10 then xs Seq.|> 1 Seq.|> (xy `mod` 10) else xs Seq.|> xy
        s'         = Seq.length xs'
        (i',j')    = ((i + x + 1) `mod` s', (j + y + 1) `mod` s')
