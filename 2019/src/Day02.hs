module Day02 where
import Data.List.Split (splitOn)
import qualified Data.Vector.Unboxed as V

parseInput :: String -> [Int]
parseInput = map read . splitOn ","

process :: V.Vector Int -> V.Vector Int
process rs =
  aux rs 0
  where
    aux registers ip =
      let
        valueA = registers V.! (registers V.! (ip + 1))
        valueB = registers V.! (registers V.! (ip + 2))
        sp     = registers V.! (ip + 3)
        opcode = registers V.! ip
      in
        case opcode of
          99 -> registers
          1  -> aux (registers V.// [(sp, valueA + valueB)]) (ip + 4)
          2  -> aux (registers V.// [(sp, valueA * valueB)]) (ip + 4)
          _  -> error "invalid opcode"

solve01 :: IO ()
solve01 = do
  i <- readFile "data/Day02.txt"
  let rs = process (V.fromList (parseInput i) V.// [(1, 12), (2, 2)])
  print (rs V.! 0)

part02 :: [Int] -> Int -> Int
part02 registers target =
  aux registers [(n, v) | n <- [0..99], v <- [0..99]]
    where
      aux :: [Int] -> [(Int, Int)] -> Int
      aux _ [] = error "could not match"
      aux rs ((n, v):ps) =
        let
          rs' = process (V.fromList rs V.// [(1, n), (2, v)])
        in
          if target == (rs' V.! 0) then
            100 * n + v
          else
            aux rs ps
        
solve02 :: IO ()
solve02 = do
  i <- readFile "data/Day02.txt"
  print (part02 (parseInput i) 19690720)
