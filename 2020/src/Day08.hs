module Day08 where

testData = "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6"

data Instruction = NOP Int | JMP Int Int | ACC Int Int deriving (Show, Eq)

parseStr :: String -> [Instruction]
parseStr = zipWith (curry parseLine) [0..] . lines
  where
    parseLine (idx, s) =
      case i of "nop" -> NOP idx
                "jmp" -> JMP idx v
                "acc" -> ACC idx v
                _     -> error ("Invalid instruction " ++ s)
        where
          s' = words s
          i = head s'
          v = intMe (head . tail $ s')
          intMe [] = error ("Invalid instruction " ++ s)
          intMe s = (read (tail s) :: Int) * (if head s == '-' then (-1) else 1)
                                      
part01 :: [Instruction] -> Int
part01 instructions =
  go 0 [] (head instructions)
  where
    go acc seen i | i `elem` seen = acc
    go acc seen i =
      case i of
        NOP idx   -> go acc       (seen ++ [i]) (instructions !! (idx + 1))
        ACC idx v -> go (acc + v) (seen ++ [i]) (instructions !! (idx + 1))
        JMP idx v -> go acc       (seen ++ [i]) (instructions !! (idx + v))

solution :: IO ()
solution = do
  instrs <- parseStr <$> readFile "data/Day08.txt"
  print (part01 instrs)
  
