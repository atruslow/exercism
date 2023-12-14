module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz 1 = Just 0
collatz n 
    | n <= 0 = Nothing
    | otherwise =  Just $ toInteger $ length $ takeWhile (> 1) (iterate colltazStep n)


colltazStep:: Integer -> Integer
colltazStep 1 = 1
colltazStep num
    | num `mod` 2 == 0 = (num `div` 2)
    | otherwise = (3 * num) + 1