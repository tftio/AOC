-- | 

module Day22 where

type Deck = [Int]

dealIntoNewStack :: Deck -> Deck
dealIntoNewStack = reverse

cutDeck :: Deck -> Int -> Deck
cutDeck d c | c == 0 = d
cutDeck d c | c >  0 = drop c d ++ take c d
cutDeck d c | c <  0 =
              let d' = reverse d in
                reverse (cutDeck d' (c * (-1)))
              

test =
  let d = [0..9] in
    dealIntoNewStack d == [9,8,7,6,5,4,3,2,1,0] &&
    cutDeck d 3 == [3,4,5,6,7,8,9,0,1,2] &&
    cutDeck d (-4) == [6,7,8,9,0,1,2,3,4,5]
    
