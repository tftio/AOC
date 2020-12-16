
module Day11 where

import qualified Data.HashMap as M
import Data.Maybe (mapMaybe)

data Seat = Occupied | Empty | NotASeat deriving (Eq, Show)
type Point = (Int, Int)
type Size  = (Int, Int)
type SeatMap = M.Map Point Seat

findAdjacentPoints :: Point -> [Point]
findAdjacentPoints (x, y) =
  filter (/= (x,y)) [(a, b) | a <- [x-1..x+1], b <- [y-1..y+1], a >= 0 && b >= 0]

parseMap :: String -> SeatMap
parseMap str =
  foldr M.union M.empty (zipWith (\y s -> M.fromList (parseLine y s)) [0..] . lines $ str)

parseLine :: Int -> String -> [(Point, Seat)]
parseLine y =
  zipWith (\x c -> ((x, y), charToSeat c)) [0..] 
  where
    charToSeat '.' = NotASeat
    charToSeat '#' = Occupied
    charToSeat 'L' = Empty
    charToSeat c   = error ("Invalid input: " ++ [c]) 

sizeOfMap :: SeatMap -> Size
sizeOfMap m = (x' + 1, y' + 1)
  where (x', y') =
          foldr (\(x, y) (xm, ym) -> (max x xm, max y ym)) (0,0) $ M.keys m

countFirstOccupiedSeats :: SeatMap -> Point -> Int
countFirstOccupiedSeats m pt@(x,y) =
  length mm
    where
    (sizeX,sizeY) = sizeOfMap m

    xs  = take sizeX [x..]
    xs' = take sizeX [x,x-1..]
    ys  = take sizeY [y..]
    ys' = take sizeY [y,y-1..]

    nw  = zip xs' ys'
    n   = zip (repeat x) ys'
    ne  = zip xs ys'

    w   = zip xs' (repeat y)
    e   = zip xs  (repeat y)

    sw  = zip xs' ys
    s   = zip (repeat x) ys
    se  = zip xs ys

    directions = map (filter (/= pt)) [nw,n,ne,w,e,sw,s,se]
    mm         = filter (Empty /=) . mapMaybe (firstChair . mapMaybe (`M.lookup` m)) $ directions
    firstChair [] = Nothing
    firstChair (NotASeat:ss) = firstChair ss
    firstChair (Empty:_) = Just Empty
    firstChair (Occupied:_) = Just Occupied

    
countAdjacentOccupiedSeats :: SeatMap -> Point -> Int
countAdjacentOccupiedSeats m =
  length . filter (Occupied ==) . mapMaybe (`M.lookup` m) . findAdjacentPoints 

updateMap :: (SeatMap -> Point -> Int) -> Int -> SeatMap -> SeatMap
updateMap counter threshold m =
  M.mapWithKey newSeatState m
  where
    occupiedSeats pt = counter m pt
    newSeatState pt state =
      case state of Occupied -> if occupiedSeats pt >= threshold then Empty else Occupied
                    Empty    -> if occupiedSeats pt == 0         then Occupied else Empty
                    NotASeat -> NotASeat
  
solve :: (SeatMap -> SeatMap) -> SeatMap -> Int    
solve updater seatMap =
  length . filter (Occupied ==) . M.elems $ go seatMap M.empty
  where
    go current prev | current == prev = current
    go current _ = go (updater current) current

solve01 :: SeatMap -> Int
solve01 = solve (updateMap countAdjacentOccupiedSeats 4)

solve02 :: SeatMap -> Int
solve02 = solve (updateMap countFirstOccupiedSeats 5)

solution :: IO ()
solution = do
  seatMap <- parseMap <$> readFile "data/Day11.txt"
  print (solve01 seatMap)
  print (solve02 seatMap)
