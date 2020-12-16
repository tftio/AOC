module Day12 where
import Debug.Trace (traceShow)

type Distance = Int
type Degree   = Int

type Point    = (Int, Int)

data Orientation = N | S | E | W deriving (Show, Eq)

data Move = North Distance |
            South Distance |
            East  Distance |
            West  Distance |
            Lft  Degree    |
            Rght Degree    |
            Forward Distance deriving (Show, Eq)

type ShipState = (Point, Orientation)

initialState :: ShipState
initialState = ((0, 0), E)

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x, y) (x', y') = abs (x - x') + abs (y - y')

testStr :: String
testStr = "F10\nN3\nF7\nR90\nF11"

testData :: [Move]
testData = parseInput testStr

parseLine :: String -> Move
parseLine s =
  case c of 'N' -> North i
            'S' -> South i
            'E' -> East i
            'W' -> West i
            'L' -> Lft i
            'R' -> Rght i
            'F' -> Forward i
            _   -> error ("invalid input " ++ s)
  where
    c = head s
    i = read (drop 1 s) :: Int

parseInput :: String -> [Move]
parseInput = map parseLine . lines

rotate :: Orientation -> Move -> Orientation
rotate o m =
  degreesToOrientation o'
  where
    d' = case m of (Lft i)  -> 360 - i
                   (Rght i) -> i
                   _        -> error "Invalid move!"
                   
    o' = case o of N -> d'
                   S -> (d' + 180) `mod` 360
                   E -> (d' + 90)  `mod` 360
                   W -> (d' + 270) `mod` 360
    degreesToOrientation 0   = N
    degreesToOrientation 90  = E
    degreesToOrientation 180 = S
    degreesToOrientation 270 = W
    degreesToOrientation _   = error "Invalid degrees"

move :: Move -> ShipState -> ShipState
move m (start@(x, y), o) =
  traceShow (m, start, o) 
  (case m of (North distance)   -> ((x, y - distance), o)
             (South distance)   -> ((x, y + distance), o)
             (West  distance)   -> ((x - distance, y), o)
             (East  distance)   -> ((x + distance, y), o)
             (Forward distance) -> move (makeMove distance) (start, o)
             _                  -> ((x, y), rotate o m))
   where
          makeMove d =
            case o of N -> North d
                      S -> South d
                      W -> West d
                      E -> East d

solve01 :: [Move] -> Int
solve01 ms =
  manhattanDistance pt pt'
  where
    (pt, _)  = foldr move initialState . reverse $ ms
    (pt', _) = initialState 

-- solve02 :: [Move] -> Int
solve02 ms =
  (x, y)
  where
    (((x, y), _), _) = foldr move2 (((0,0), E), (10, -1)) $ reverse ms

type ExpandedShipState = (ShipState, Point) -- we no longer need an orientation, right?

move2 :: Move -> ExpandedShipState -> ExpandedShipState
move2 m es@(ss@((x,y), o), wp@(x',y')) =
  case m of
    (North d) -> (ss, (x', y' - d)) 
    (South d) -> (ss, (x', y' + d))
    (East  d) -> (ss, (x' + d, y'))
    (West  d) -> (ss, (x' - d, y'))
    (Forward d) -> moveToWayPoint es d
    _           -> (((x,y), o), rotateWayPoint wp m)
  where
    moveToWayPoint (((x,y), o), (x',y')) d =
      (((x + (d * x'), y + (d * y')), o), (x', y'))

rotateWayPoint :: Point -> Move -> Point
rotateWayPoint (x, y) m = (xd, yd)
  where
    (xd, yd) = case m of (Rght 90)   -> (-y, x)
                         (Rght 180)  -> (-x, -y)
                         (Rght 270)  -> (y, -x)
                         (Rght 360)  -> (x, y)
                         (Lft 90)  -> (y, -x)
                         (Lft 180) -> (-x, -y)
                         (Lft 270) -> (-y, x)
                         (Lft 360) -> (x, y)
                         
                         _          -> error ("Invalid move: " ++ show m)
      
  
  

solution :: IO ()
solution = do
  ms <- parseInput <$> readFile "data/Day12.txt"
  print (solve01 ms)
  print (solve02 ms)
