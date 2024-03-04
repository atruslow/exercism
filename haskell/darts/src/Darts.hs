module Darts (score) where


data DartPosition = Outside | OuterCircle | MiddleCircle | InnerCircle deriving (Enum, Show, Eq)

score :: Float -> Float -> Int
score x y = case get_position x y of
    Outside -> 0
    OuterCircle -> 1
    MiddleCircle -> 5
    InnerCircle -> 10

distance :: Float -> Float -> Float
distance x y = sqrt( (x ** 2.0) + (y ** 2.0) )

get_position :: Float -> Float -> DartPosition
get_position x y
    | dist > 10 = Outside
    | dist > 5 = OuterCircle
    | dist > 1 = MiddleCircle
    | otherwise = InnerCircle
    where dist = distance x y
