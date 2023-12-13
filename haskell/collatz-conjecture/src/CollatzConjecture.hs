module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz 1 = Just 0
collatz n 
    | n <= 0 = Nothing
    | otherwise = Just (colltaz_steps 0 n)


colltaz_steps:: Integer -> Integer -> Integer
colltaz_steps step_count 0 = step_count
colltaz_steps step_count 1 = step_count
colltaz_steps step_count num
    | num `mod` 2 == 0 = colltaz_steps new_step_count (num `div` 2)
    | otherwise = colltaz_steps new_step_count ((3 * num) + 1)
    where new_step_count = step_count + 1
