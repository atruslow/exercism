{-# OPTIONS_GHC -Wall #-}

module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Text.Read as Read

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Read)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = do 
    dnaStr <- mapM toNucleotide xs
    Right (Map.fromListWith (+) [(x, 1) | x <- dnaStr])

toNucleotide :: Char -> Either String Nucleotide
toNucleotide x = Read.readEither [x]