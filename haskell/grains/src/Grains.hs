module Grains (square, total) where

square :: Integer -> Maybe Integer
square n 
    | n <= 0 = Nothing
    | n > 64 = Nothing
    | otherwise = Just (2 ^ (n-1))

total :: Integer
total = case x of
        Just x -> sum(x)
        Nothing -> 0
    where x = mapM (\x->square x) (take 64 (iterate (+1) 1))
