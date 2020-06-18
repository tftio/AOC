module Day08 where

testData = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
testSolution = 138

getData = (map readInt) . words  where
  readInt n = read n :: Int

data Node = Node [Int] [Node] | Empty deriving Show
data Header = Header Int Int deriving Show

headerFromList :: [Int] -> Header
headerFromList (numKids:sizeMD:_) = Header numKids sizeMD
headerFromList _                  = error "Malformed list"

buildTree :: [Int] -> Node
buildTree = bt Empty 0 where
  bt _ _ _ = Empty

