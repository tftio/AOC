module Day11 where
import           Data.List

serial = 7403

powerLevel x y = let
                    rack = x+10
                    a = (rack * y + serial)*rack
                  in
                    ((a `div` 100) `mod` 10) - 5

powerSquare (x, y, n) = sum [powerLevel z w | z <- [x..x+(n-1)], w <- [y..y+(n-1)] ]

comparePower a b = compare (powerSquare a) (powerSquare b)

best :: [Int] -> (Int, Int, Int)
best range = maximumBy comparePower [(x,y,n)| n <- range,x <- [1..(301-n)], y <- [1..(301-n)]]

-- handleOne l = do
--    let a = best [l]
--    putStrLn $ (show l) ++ ": " ++ show a ++ " " ++ show (powerSquare a)

solve02 = do
    print $ best [3]
    print $ best [1..30]
