-- |

module Day06 where
import           Data.List.Split                ( splitOn )
import qualified Data.Map.Strict               as Map
import           Data.List
-- from the body /that/ orbits to the body it orbits.
type Orbits = Map.Map String String

parseInput :: String -> Orbits
parseInput = Map.fromList . map (toPair . reverse . splitOn ")") . lines
 where
  toPair [a, b] = (a, b)
  toPair bogus  = error ("received bogus input: " ++ show bogus)

orbitCount :: Orbits -> String -> Integer
orbitCount orbits planet | Map.member planet orbits =
  1 + orbitCount orbits (orbits Map.! planet)
orbitCount _ _ = 0

testDataStr :: String
testDataStr =
  "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"

testData :: Orbits
testData = parseInput testDataStr

-- get the intersection of the paths from a, b to the common ancestor
-- measure the length of that intersection
-- done.

type OrbitalPath = [String]

derivePath :: Orbits -> String -> String -> OrbitalPath
derivePath _ from to | from == to = []
derivePath orbits from to         = go from []
 where
  go from' acc | from' == to = to : acc
  go from' acc               = go (orbits Map.! from') (from' : acc)

distanceBetween :: Orbits -> String -> String -> Int
distanceBetween orbits a b = a'' + b'' - 2
 where
  a'' = length (a' \\ b')
  b'' = length (b' \\ a')
  a'  = derivePath orbits a "COM"
  b'  = derivePath orbits b "COM"
  -- Set.difference (derivePath orbits a "COM") (derivePath orbits b "COM")

solve01 :: IO ()
solve01 = do
  i <- readFile "data/Day06.txt"
  let orbits = parseInput i
  print (sum $ map (orbitCount orbits) $ Map.keys orbits)

solve02 :: IO ()
solve02 = do
  i <- readFile "data/Day06.txt"
  print (distanceBetween (parseInput i) "YOU" "SAN")
