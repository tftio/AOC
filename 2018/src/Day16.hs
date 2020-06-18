{-# LANGUAGE OverloadedStrings #-}
module Day16 where
import Data.Bits ((.|.), (.&.))
import Data.Maybe (mapMaybe)
import Data.List.Split
import Data.List (groupBy, sortBy, partition)
import qualified Data.Map as Map

import qualified Data.Set as Set

data Opcode   = ADDR | ADDI | MULR | MULI | BANR | BANI | BORR | BORI | SETR | SETI | GTIR | GTRI | GTRR | EQIR | EQRI | EQRR | Unknown Int deriving (Show, Eq, Ord)
data Register = Register Int deriving (Show, Eq, Ord)

data Instruction = Instruction Opcode Register Register Register deriving (Show, Eq, Ord)
data State       = State Register Register Register Register     deriving (Show, Eq, Ord)

type UnknownInstruction = (State, Instruction, State)

opcodeList = [ADDR, ADDI, MULR, MULI, BANR, BANI, BORR, BORI, SETR, SETI, GTIR, GTRI, GTRR, EQIR, EQRI, EQRR]

emptyState = State (Register 0) (Register 0) (Register 0) (Register 0)

registerAt :: State -> Int -> Register
registerAt (State a b c d) i =
  case i of 0 -> a
            1 -> b
            2 -> c
            3 -> d
            _ -> error ("Invalid register: " ++ show i)

updateState :: State -> Register -> Int -> State
updateState state@(State a b c d) (Register idx) val =
  case idx of
    0 -> State (Register val) b c d
    1 -> State a (Register val) c d
    2 -> State a b (Register val) d
    3 -> State a b c (Register val) 
    _ -> error ("invalid index: " ++ show idx ++ " into state " ++ show state)

execute :: State -> Instruction -> State
execute state (Instruction opcode a b c) =
  updateState state c val
  where
    val = case opcode of
      ADDR -> deref a + deref b
      ADDI -> deref a + bare b
      MULR -> deref a * deref b
      MULI -> deref a * bare b
      BANR -> deref a .&. deref b
      BANI -> deref a .&. bare b
      BORR -> deref a .|. deref b
      BORI -> deref a .|. bare b
      SETR -> deref a
      SETI -> bare a
      GTIR -> if bare a > deref b then 1 else 0
      GTRI -> if deref a > bare b then 1 else 0
      GTRR -> if deref a > deref b then 1 else 0
      EQIR -> if bare a == deref b then 1 else 0
      EQRI -> if deref a == bare b then 1 else 0
      EQRR -> if deref a == deref b then 1 else 0
      _ -> error ("invalid state: " ++ show opcode)
    deref (Register v) = (bare . registerAt state) v
    bare  (Register v) = v

findPossibleMatches :: UnknownInstruction -> (Int, Set.Set Opcode)
findPossibleMatches (pre, (Instruction (Unknown i) a b c), post) =
  (i, Set.fromList (mapMaybe m opcodeList))
  where
    m opcode = if execute pre (Instruction opcode a b c) == post
               then
                 Just opcode
               else
                 Nothing

testData = (State (Register 3) (Register 2) (Register 1) (Register 1),
            Instruction (Unknown 9) (Register 2) (Register 1) (Register 2),
            State (Register 3) (Register 2) (Register 2) (Register 1))

testStr = "Before: [3, 2, 1, 1]\n9 2 1 2\nAfter:  [3, 2, 2, 1]\n"
splitter = split (condense .dropDelims . dropInnerBlanks . dropInitBlank . dropFinalBlank $ oneOf "abcdefghijklmnopqrstuvwxyz=<>,:[] ")    
toInt c = read c :: Int

parsePart01 :: String -> [UnknownInstruction]
parsePart01 str =
  parse Nothing Nothing Nothing [] ss
  where
    ss = (map splitter . filter (\l -> (length l) > 1) . lines) str

    lineToState :: [String] -> State
    lineToState ss | (length ss) == 5 && ((head ss) == "A" || (head ss) == "B") = State (Register a) (Register b) (Register c) (Register d)
      where (a:b:c:d:[]) = map toInt (tail ss)
    lineToState l = error ("Cannot parse as state: " ++ (show l))

    lineToInstr :: [String] -> Instruction
    lineToInstr ss | (length ss) == 4 = Instruction (Unknown opcode) (Register a) (Register b) (Register c)
      where (opcode:a:b:c:[]) = map toInt ss
    lineToInstr l = error ("Cannot parse as instr: " ++ (show l))    
    
    parse :: Maybe State -> Maybe Instruction -> Maybe State -> [UnknownInstruction] -> [[String]] -> [UnknownInstruction]
    parse (Just pre) (Just instr) (Just post) acc [] = acc ++ [(pre, instr, post)]
    parse _ _ _ acc [] = acc
    parse (Just pre) (Just instr) (Just post) acc ss = parse Nothing Nothing Nothing (acc ++ [(pre, instr, post)]) ss
    parse Nothing Nothing Nothing acc (l:ls) = parse (Just (lineToState l)) Nothing Nothing acc ls
    parse pre     Nothing Nothing acc (l:ls) = parse pre (Just (lineToInstr l)) Nothing acc ls
    parse pre     instr   Nothing acc (l:ls) = parse pre instr (Just (lineToState l)) acc ls
  
solve01 = length . (filter (\(_, ops) -> (length ops) > 2)) . (map findPossibleMatches) . parsePart01
solve02 part01str part02str  =
  foldr (\i s -> execute s i) (State (Register 0) (Register 0) (Register 0) (Register 0)) instructions
  where
    instructions = (map parseLine . (filter (\l -> length l > 2)) . lines) part02str
    parseLine l =
      let (o:a:b:c:[]) = (map toInt . splitter) l
      in Instruction (instructionMap Map.! o) (Register a) (Register b) (Register c)
    instructionMap =
      (clean [] .
       map cruncher .
       groupBy (\(i, _) (i', _) -> i == i') .
       sortBy (\(i, _) (i', _) -> compare i i') .
       map findPossibleMatches .
       parsePart01) part01str
    cruncher l = (o, combiner s)
      where
        (o:_, s) = unzip l
        combiner (s:ss) = foldr (\a b -> Set.intersection a b) s ss

    clean solved [] = Map.fromList (map (\(o, s) -> (o, Set.elemAt 0 s)) solved)
    clean solved unsolved = clean (solved ++ s') u'
      where
        solvedSet = Set.unions (map snd solved)
        (s', u') = partition (\(_, s) -> Set.size s == 1) (map (\(i, s) -> (i, Set.difference s solvedSet)) unsolved)

go :: IO()
go = do
  p1 <- readFile "data/Day16.txt"
  let [pt1, pt2] = splitOn "\n\n\n\n" p1
  print(show (solve01 pt1))
  print(show (solve02 pt1 pt2))
  
  
