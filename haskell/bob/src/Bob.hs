module Bob (responseFor) where

import Data.Char (isUpper, isAlpha)
import Data.Char (isSpace, isDigit, isAlphaNum)


responseFor :: String -> String
responseFor xs
    | xs == "" || all isSpace xs = "Fine. Be that way!" 
    | last xs == '?' && (filter isAlphaNum xs) == "" = "Sure."
    | last xs == '?' && all isUpper (filter isAlphaNum xs) = "Calm down, I know what I'm doing!"
    | last (trim xs) == '?' = "Sure."
    | all isDigit (filter isAlphaNum xs) = "Whatever."
    | all isUpper (filter isAlpha xs) = "Whoa, chill out!"
    | otherwise = "Whatever."



trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace