module Day08 where

import Data.Vector (Vector, (!), (!?), (//))
import qualified Data.Vector as V

testStr :: String
testStr = "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6"

testData :: Program
testData = parseStr testStr

data Instruction = NOP Int | JMP Int | ACC Int deriving (Show, Eq)
data Result = Cycle Int | Termination Int deriving (Show, Eq)
type Program = Vector Instruction

parseStr :: String -> Program
parseStr = V.fromList . map parseLine . lines
  where
    parseLine s =
      case i of "nop" -> NOP v
                "jmp" -> JMP v
                "acc" -> ACC v
                _     -> error ("Invalid instruction " ++ s)
        where
          s' = words s
          i = head s'
          v = intMe (head . tail $ s')
          intMe str = (read (tail str) :: Int) * (if head str == '-' then (-1) else 1)
                                      
run :: Program -> Result
run instructions =
  go 0 [] 0
  where
    go acc seen i | i `elem` seen = Cycle acc
    go acc _    i | i >= V.length instructions = Termination acc
    go acc seen i =
      case instructions ! i of
        NOP _ -> go acc       (seen ++ [i]) (i + 1)
        ACC v -> go (acc + v) (seen ++ [i]) (i + 1)
        JMP v -> go acc       (seen ++ [i]) (i + v)

part01 :: Program -> Result
part01 = run

part02 :: Program -> Result
part02 program = head .
                 filter isOk .
                 map run .
                 zipWith (\idx istr -> program // [(idx, switch istr)]) [0..] .
                 V.toList $ program 
  where
    isOk (Termination _) = True
    isOk _               = False
    switch (NOP i) = JMP i
    switch (JMP i) = NOP i
    switch i       = i 

solution :: IO ()
solution = do
  instrs <- parseStr <$> readFile "data/Day08.txt"
  print (part01 instrs)
  print (part02 instrs)     
  
