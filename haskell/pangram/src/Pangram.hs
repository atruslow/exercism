module Pangram (isPangram) where

import Data.List (isInfixOf)
import Data.Char (toLower, isAlpha)
import qualified Data.Set as Set

isPangram :: String -> Bool
isPangram text = 
    let aplhaSet = Set.fromList ['a'..'z'] in
        let filteredTextSet = Set.fromList (map toLower (filter isAlpha text)) in 
            all (\letter -> letter `elem` filteredTextSet) aplhaSet



