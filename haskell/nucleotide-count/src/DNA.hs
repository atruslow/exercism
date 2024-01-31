module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Read)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = case mapM toNucleotide xs of
    Just dnaStr -> Right (Map.fromListWith (+) [(x, 1) | x <- dnaStr])
    Nothing -> Left "Invalid"

toNucleotide :: Char -> Maybe Nucleotide
toNucleotide 'A' = Just A
toNucleotide 'C' = Just C
toNucleotide 'G' = Just G
toNucleotide 'T' = Just T
toNucleotide _ = Nothing

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = Map.toList (Map.fromListWith (+) [(x, 1) | x <- xs])