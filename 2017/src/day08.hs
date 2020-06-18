module Day08 where

import qualified Data.Map
import Data.Maybe

type BinOp = Int -> Int -> Bool
type Register = String
data Mod = Inc Int | Dec Int deriving Show
data Cond = Lt Register Int  |
            Gt Register Int  |
            Le Register Int  |
            Ge Register Int  |
            Ne Register Int  |
            Eq Register Int  deriving Show

type Instruction = (Register, Mod, Cond)
type State = Data.Map.Map Register Int

strToInstr str =
  case words str of
    (target:mod:magnitude:_:lside:op:rside:[]) -> case (c, m) of
      (Nothing, _) -> Nothing
      (_, Nothing) -> Nothing
      (Just c', Just m') -> Just (target, m', c')
      where
        r   = read rside :: Int
        mag = read magnitude :: Int
        m = case mod of "inc" -> Just (Inc mag)
                        "dec" -> Just (Dec mag)
                        _     -> Nothing
        c = case op of
          "<"  -> Just (Lt lside r)
          ">"  -> Just (Gt lside r)
          "<=" -> Just (Le lside r)
          ">=" -> Just (Ge lside r)
          "!=" -> Just (Ne lside r)
          "==" -> Just (Eq lside r)
          _    -> Nothing
    _ -> Nothing

execute :: (Int, State) -> Instruction -> (Int, State)
execute (currentMax, state) (target, mod, condition) =
  if shouldEval then
    (newMax, setRegister target newVal)
  else
    (newMax, state)
  where
    newMax          = max currentMax (maxOfState state)
    currentVal      = getRegister target
    getRegister r   = Data.Map.findWithDefault 0 r state
    setRegister r v = Data.Map.insert r v state

    shouldEval = case condition of
      Lt r v -> getRegister r < v
      Gt r v -> getRegister r > v
      Le r v -> getRegister r <= v
      Ge r v -> getRegister r >= v
      Ne r v -> not (getRegister r == v)
      Eq r v -> getRegister r == v

    newVal = case mod of Inc i -> currentVal + i
                         Dec i -> currentVal - i

maxOfState :: State -> Int
maxOfState = foldr (\m m' -> max m m') 0 . Data.Map.foldr (\w acc -> w:acc) []

testInput = "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10\n"
run = foldl execute (0, Data.Map.empty) . mapMaybe strToInstr . lines

isOk = run testInput == (10, Data.Map.fromList [("a",1),("c",-10)])

solve i =
  (foldr (\m m' -> max m m') 0 (Data.Map.foldr (\w acc -> w:acc) [] state), m)
  where
    (m, state) = run i

main = do
  i <- readFile "/Users/jamesblack/Projects/AOC-2017/data/Day08.txt"
  print(solve i)
