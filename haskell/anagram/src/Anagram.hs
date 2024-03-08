module Anagram (anagramsFor) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = [ inputStr | (inputStr, _, _) <- matchingSets ]
    where valueSet = Set.fromList (lower xs)
          valueSort = (sort $ lower xs)
          inputSets = [(c, Set.fromList (lower c), (sort (lower c))) | c <- xss]
          matchingSets = filter (\(inputStr, charSet, sortStr) -> valueSet == charSet && valueSort == sortStr && not ( lower xs ==  lower inputStr )) inputSets

lower :: String -> String
lower xs = [toLower x | x <- xs]