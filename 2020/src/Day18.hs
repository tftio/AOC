-- |
module Day18 where

import Data.Char (digitToInt, isDigit)

testData :: [(String, Int)]
testData =
  [ ("1 + 2 * 3 + 4 * 5 + 6", 71),
    ("1 + (2 * 3) + (4 * (5 + 6))", 51),
    ("2 * 3 + (4 * 5)", 26),
    ("5 + (8 * 3 + 9 + 3 * 4 * 3)", 437),
    ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 12240),
    ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 13632)
  ]

testStr = fst . head . tail $ testData

test :: Bool
test = all (\(s, v) -> solve s == v) testData

solve :: String -> Int
solve = eval . parse . mangle

data Expr = Digit Int | Add Expr Expr | Mult Expr Expr deriving (Show, Eq)

eval :: Expr -> Int
eval (Digit i) = i
eval (Add a b) = eval a + eval b
eval (Mult a b) = eval a * eval b

parse :: String -> Expr
parse [c] = Digit $ digitToInt c
parse s = case stripTerm s of
  (first, "") -> parse first
  (first, rest) ->
    let op = head rest
        second = tail rest
     in case op of
          '+' -> Add (parse first) (parse second)
          '*' -> Mult (parse first) (parse second)

toTerms str =
  crunch [] []
  where
    crunch ct at str = mangle . reverse $ str

mangle :: String -> String
mangle = go ""
  where
    go acc "" = acc
    go acc (c : cs) | c == ' ' = go acc cs
    go acc (c : cs) | c == ')' = go ("(" ++ acc) cs
    go acc (c : cs) | c == '(' = go (")" ++ acc) cs
    go acc (c : cs) = go (c : acc) cs

testTree = Add (Digit 6) (Mult (Digit 5) (Add (Digit 4) (Mult (Digit 3) (Add (Digit 2) (Digit 1)))))

testTree2 = Add (Digit 1) (Add (Mult (Digit 2) (Digit 3)) (Mult (Digit 4) (Add (Digit 5) (Digit 6))))

solve01 = sum . map solve . lines

findMatching :: String -> Int -> String
findMatching ('(' : cs) rem = '(' : findMatching cs (rem + 1)
findMatching (')' : cs) rem =
  if rem == 1
    then ""
    else ')' : findMatching cs (rem - 1)
findMatching (c : cs) rem = c : findMatching cs rem

headTerm :: String -> String
headTerm (c : cs)
  | c == '(' = findMatching cs 1
  | otherwise = [c]

stripTerm :: String -> (String, String)
stripTerm s =
  let t = headTerm s
      l = if head s == '(' then length t + 2 else 1
   in (t, drop l s)

mangle2 :: String -> String
mangle2 =
  ('(' :)
    . (++ ")")
    . concatMap bracketPrecedence
    . reverse
    . filter (/= ' ')
  where
    bracketPrecedence c
      | c == '(' = "))"
      | c == ')' = "(("
      | c == '*' = ")*("
      | otherwise = [c]

main = do
  str <- readFile "data/Day18.txt"
  print $ sum . map (eval . parse . mangle) . lines $ str
  print $ sum . map (eval . parse . mangle2) . lines $ str
