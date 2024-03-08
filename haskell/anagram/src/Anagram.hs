{-# OPTIONS_GHC -Wall #-}

module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as Set

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = [inputStr | (inputStr, _, _) <- matchingSets]
 where
  matchingSets = filter isMatch xs y inputSets
  inputSets = [(c, Set.fromList (lower c), sort (lower c)) | c <- xss]

isMatch :: String -> (String, Set Char, String) -> Bool
isMatch xs (inputStr, charSet, sortStr) = valueSet == charSet && valueSort == sortStr && lower xs /= lower inputStr
 where
  valueSort = sort $ lower xs
  valueSet = Set.fromList (lower xs)

lower :: String -> String
lower xs = [toLower x | x <- xs]