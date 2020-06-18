-- |

module Day07 where
-- god not more of this intcode garbage

import           Day05                          ( parseInput
                                                , runProgram
                                                , Program
                                                , ProgramState
                                                , ProgramStatus(..)
                                                )
import qualified Data.Vector.Unboxed           as V
import           Data.List                      ( permutations )

testData :: Program
testData = parseInput "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"


testData2 :: Program
testData2 =
  parseInput
    "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"

findMaxSignal :: Program -> Int
findMaxSignal p =
  maximum . map (foldr (\f i -> f i) 0 . reverse . map aux) $ permutations
    [0 .. 4]
 where
  aux i = extractOutput . exec p . makeList i
   where
    exec program input = runProgram (program, 0, input, V.empty)
    makeList a b = a : [b]
    extractOutput :: ProgramStatus -> Int
    extractOutput (Finished (_, _, _, o)) = V.head o
    extractOutput v = error ("Cannot determine wtf is " ++ show v)

-- ok, for part two:
-- run the first with the init input
-- if Halt -> _
-- if WaitingForInput _ then send this into the next iteration

solve01 :: IO ()
solve01 = do
  i <- readFile "data/Day07.txt"
  print (findMaxSignal . parseInput $ i)
