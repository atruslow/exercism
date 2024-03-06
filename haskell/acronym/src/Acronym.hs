module Acronym (abbreviate) where

import qualified Data.Text as T
import           Data.Text (Text)

import Data.Char as C

abbreviate :: String -> String
abbreviate xs = T.unpack . T.concat $ map (T.toUpper . T.take 1)  (parseWords xs)

parseWords :: String -> [Text]
parseWords = T.words . T.pack . insertSpaceBeforeCapital . removePunc . truncateApostrophes

insertSpaceBeforeCapital :: String -> String
insertSpaceBeforeCapital xs = [head xs] ++ concatMap (\(y,z) -> if (C.isLower y && C.isUpper z) then " " ++ [z] else [z]) (zip xs (drop 1 xs))

truncateApostrophes :: String -> String
truncateApostrophes xs = [ x | x <- xs, not (x `elem` "\'") ]

removePunc :: String -> String
removePunc xs = map (\y -> if isPunc y then ' ' else y) xs
 
isPunc :: Char -> Bool
isPunc x =  x `elem` ",.?!-:;\"\'_"

