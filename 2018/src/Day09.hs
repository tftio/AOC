module Day09 where
import           Data.List.Split
import qualified Data.Vector     as V

type Board  = V.Vector Int
type Score  = Int
type Marble = Int
type Player = Int

data Game = Game { playerScores :: V.Vector Int, board :: Board } deriving (Show, Eq)

solve :: [Char] -> Int
solve _ = 0

parseInput :: [Char] -> (Int, Int)
parseInput str = case xs of (a:b:[]) -> (read a :: Int, read b :: Int)
                            _        -> error "invalid input"
  where
    xs = split (dropInitBlank . dropFinalBlank . condense . dropDelims $ oneOf "abcdefghijklmnopqrstuvwxyz ;") str

rotate :: Int -> V.Vector a -> V.Vector a
rotate steps v | V.length v > 1 = (V.++) (V.drop steps'' v) (V.take steps'' v)
               | True           = v
  where steps' | steps >= 0 = steps
               | steps < 0  = ((V.length v) + steps)
        steps''             = steps' `mod` (V.length v)

emptyGame players = Game { playerScores = V.replicate players 0, board = V.singleton 0 }

addMarble :: Marble -> Board -> (Int, Board)
addMarble m b | (m `mod` 23) /= 0  = (0, V.cons m (rotate 2 b))
              | (m `mod` 23) == 0  = (m + newM, newBoard)
                where rotatedBoard = rotate (-7) b
                      newM         = V.head rotatedBoard
                      newBoard     = V.tail rotatedBoard

playTurn :: Game -> Int -> Game
playTurn game marble =
  Game { playerScores = newScores, board = newBoard }
  where player = marble `mod` (V.length ss)
        ss = playerScores game
        b  = board game
        oldScore = ss V.! player
        (s, newBoard) = addMarble marble b
        newScores = ss V.// [(player, s + oldScore)]

playGame :: Int -> Int -> Game
playGame numPlayers numTurns =
  foldl playTurn (emptyGame numPlayers) [1..numTurns]

highScore :: Game -> Int
highScore g = V.maximum (playerScores g)

testData = "foo"
part01Data = "418 players; last marble is worth 7133900 points"
solution01 = highScore (playGame players turns) where (players, turns) = parseInput part01Data
