module Day07 where

import qualified Data.Graph as G
import qualified Data.List

tree = foldr unwrap [] . map nodeFromStr . lines
  where
    unwrap n acc =
      case n of Just (n, w, ks) -> ((n, w), n, ks):acc
                Nothing -> acc
    nodeFromStr line =
      case words line of
        (name:w:[]) -> Just (name, read w :: Int, [])
        (name:w:ks) -> Just (name, read w :: Int, map e (tail ks))
        _           -> Nothing
      where
        e = filter (\c -> not (c == ','))

asG = G.graphFromEdges  . tree
solveOne i = name
  where
   (g, f, _) = asG i
   (_, name, _) = head (map f (G.topSort g))

getNode n (g, f, f') =
  case f' n of
    Nothing -> (0, [])
    Just i  -> case (f i) of
      ((_, w), _, kids) -> (w, kids)

-- given a node N, return the list [(n', w)] where [n'] is the list of children of node N

weightOfNode node graph = w  where (w,  _) = getNode node graph
kidsOfNode   node graph = ks where (_, ks) = getNode node graph

weightOfTree node graph =
  aux weight kids
  where
    (weight, kids) = getNode node graph
    aux sum []     = sum
    aux sum (n:ns) = aux (sum + w) (ns ++ ks)
      where
        (w, ks) = getNode n graph

balancedNode node graph =
  length (Data.List.groupBy (==) weights) <= 1
  where
    weights = case getNode node graph of
                (_, ks) -> map (\n -> case getNode n graph of (w, _) -> w) ks


main = do
  i <- readFile "/Users/jamesblack/Projects/AOC-2017/data/Day07.txt"
  let g = asG i
  let top = solveOne i
  let r n = map (\n' -> (n', weightOfTree n' g, weightOfNode n' g)) (kidsOfNode n g)
  print (r top)
  print (r "dwggjb")
  print (r "wknuyhc")
  print (r "egbzge")


testInput = "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)\n  foo"


