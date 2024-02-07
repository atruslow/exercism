module SumOfMultiples (sumOfMultiples) where

import Data.Set (Set)
import qualified Data.Set as Set


sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum (combined_factors)
    where combined_factors = mkUniq (concat ( map (\x -> getFactors x limit ) factors ))


getFactors :: Integer -> Integer -> [Integer]
getFactors 0 _ = [0]
getFactors x limit = takeWhile (< limit) (iterate (+ x) x)

mkUniq :: Ord a => [a] -> [a]
mkUniq = Set.toList . Set.fromList