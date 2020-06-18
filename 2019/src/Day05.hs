-- | more shit

module Day05
  ( parseInput
  , runProgram
  , ProgramStatus(..)
  , ProgramState
  , Program
  , Input
  , Output
  )
where
import           Data.List.Split                ( splitOn )
import qualified Data.Vector.Unboxed           as V
import           Data.Digits                    ( digitsRev )

parseInput :: String -> Program
parseInput = V.fromList . map read . splitOn ","

type Program = V.Vector Int
type Opcode = Int
type Input = [Int]
type Output = V.Vector Int
type InstructionPointer = Int
type Address = Int
type Value = Int

type ProgramState = (Program, InstructionPointer, Input, Output)

data ProgramStatus = Finished ProgramState | WaitingForInput ProgramState | Continue ProgramState deriving Show

data Mode = Immediate | Position deriving Show
data Instruction = Add | Mult | Input | Output | Halt | JIT | JIF | LessThan | Equal deriving Show

decode :: Opcode -> (Instruction, [Mode])
decode opcode = case op of
  [9, 9] -> (Halt, [])
  [1, 0] -> (Add, take 2 params)
  [2, 0] -> (Mult, take 2 params)
  [3, 0] -> (Input, take 1 params)
  [4, 0] -> (Output, take 1 params)
  [5, 0] -> (JIT, take 2 params)
  [6, 0] -> (JIF, take 2 params)
  [7, 0] -> (LessThan, take 2 params)
  [8, 0] -> (Equal, take 2 params)
  _      -> error ("invalid opcode: " ++ show opcode)
 where
  ds           = digitsRev 10 opcode ++ repeat 0
  (op, params) = (take 2 ds, map makeParams (drop 2 ds))
  makeParams v = case v of
    1 -> Immediate
    0 -> Position
    _ -> error ("Invalid param: " ++ show v)


load :: Program -> Address -> Mode -> Value
load program addr mode = case mode of
  Immediate -> deref program addr
  Position  -> program V.! deref program addr

-- TODO: stores always in position mode
store :: Program -> Address -> Value -> Program
store program addr val = program V.// [(deref program addr, val)]

deref :: Program -> Address -> Value
deref program addr = program V.! addr

-- TODO: JIT / JIF are b0rked, something to do with Position mode
processInstruction :: ProgramState -> ProgramStatus
processInstruction (program, ip, input, output) =
  let (opcode, paramTypes) = decode (program V.! ip)
      inputValues          = zipWith (load program) [ip + 1 ..] paramTypes
      conditional op =
          ( store program (ip + 3) (if op a b then 1 else 0)
          , ip + 4
          , input
          , output
          )
         where
          a = head inputValues
          b = head . tail $ inputValues
      computeNewIP op =
          if op (head inputValues) then head . tail $ inputValues else ip + 3
  in  case opcode of
        Add -> Continue
          (store program (ip + 3) (sum inputValues), ip + 4, input, output)
        Mult ->
          Continue
            ( store program (ip + 3) (product inputValues)
            , ip + 4
            , input
            , output
            )
        Input -> case input of
          [] -> WaitingForInput (program, ip, [], output)
          i : input' ->
            Continue (store program (ip + 1) i, ip + 2, input', output)
        Output -> Continue
          ( program
          , ip + 2
          , input
          , V.snoc output (load program (ip + 1) (head paramTypes))
          )
        JIT      -> Continue (program, computeNewIP (/= 0), input, output)
        JIF      -> Continue (program, computeNewIP (== 0), input, output)
        LessThan -> Continue (conditional (<))
        Equal    -> Continue (conditional (==))
        Halt     -> Finished (program, ip, input, output)

runProgram :: ProgramState -> ProgramStatus
runProgram v = case processInstruction v of
  Continue v' -> runProgram v'
  rv          -> rv
