module Day14 where

import Data.Bits (setBit, clearBit)
import Text.Regex.TDFA
import qualified Data.HashMap as M

type Bitmask = [(Int, Char)]
type State   = M.Map Int Integer

data Instruction = Store Int Integer | SetBitmask Bitmask deriving (Eq, Show)

testInput :: String
testInput = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\nmem[8] = 11\nmem[7] = 101\nmem[8] = 0"

testInput2 :: String
testInput2 = "mask = 000000000000000000000000000000X1001X\nmem[42] = 100\nmask = 00000000000000000000000000000000X0XX\nmem[26] = 1"

testProgram2 :: [Instruction]
testProgram2 = parseInput testInput2

emptyBitmask :: Bitmask
emptyBitmask = []

expandBitmask :: Bitmask -> [Bitmask]
expandBitmask bm = [bm]

parseInput :: String -> [Instruction]
parseInput =
  map f . lines
  where
    f s = case take 4 s of "mask" -> lineToBitmask s
                           "mem[" -> lineToStore s
                           _      -> error ("Invalid instruction: " ++ s)

lineToBitmask :: String -> Instruction                           
lineToBitmask = SetBitmask . reverse  . zip [35,34..] . drop 7

lineToStore :: String -> Instruction
lineToStore s = Store (read loc) (read val)   
  where
    (_, _, _, [loc, val]) = s =~ "^mem\\[([0-9]+)\\] = ([0-9]+)$" :: (String, String, String, [String])

applyBitmask_part1 :: Bitmask -> Instruction -> State -> State
applyBitmask_part1 _  (SetBitmask _)  state = state 
applyBitmask_part1 bm (Store loc val) state =
  M.insert loc val' state
  where
    f (b, '0') i = clearBit i b
    f (b, '1') i = setBit   i b 
    f _ _        = error ("Invalid bitmask " ++ show bm)
    val' = foldr f val (filter ((/=) 'X' . snd) bm)

applyBitmask_part2 :: Bitmask -> Instruction -> State -> State
applyBitmask_part2 _ (SetBitmask _) state = state
applyBitmask_part2 bm (Store loc val) state =
  M.union state' state
  where
    state' = M.fromList (zip (gatherAddresses bm loc) (repeat val))
    toBits :: Int -> [Int]
    toBits num = take 36 (helper num ++ repeat 0)
      where
        helper 0 = []
        helper n = let (q,r) = n `divMod` 2 in r : helper q

    gatherAddresses bitmask loc = map fromBits $ go (combine bitmask loc) [[]]
      where
        go []     acc = acc
        go (c:bs) acc =
          case c of 'X' -> go bs ((map ([1] ++) acc) ++ (map ([0] ++) acc))
                    '1' -> go bs (map ([1] ++) acc)
                    '0' -> go bs (map ([0] ++) acc)
                    _   -> error "whoa"

    combine bitmask value =
      zipWith (\(_, c) b -> case c of 'X' -> 'X'
                                      '1' -> '1'
                                      '0' -> if b == 1 then '1' else '0'
                                      _   -> error "whoa")
        bitmask (toBits value)

    fromBits ints = sum $ zipWith (\b e -> b * 2 ^ e) ints [(length ints - 1),(length ints - 2)..]

run :: (Bitmask -> Instruction -> State -> State) -> [Instruction] -> State
run applier instrs =
  go emptyBitmask M.empty instrs
  where
    go _       state [] = state
    go bitmask state (i:is) =
      case i of SetBitmask m -> go m       state                     is
                Store _ _    -> go bitmask (applier bitmask i state) is

solve01 :: [Instruction] -> Integer
solve01 = M.fold (+) 0 . run applyBitmask_part1

solve02 :: [Instruction] -> Integer
solve02 = M.fold (+) 0 . run applyBitmask_part2

solution :: IO ()  
solution = do
  instrs <- parseInput <$> readFile "data/Day14.txt"
  print (solve01 instrs)
  print (solve02 instrs)

