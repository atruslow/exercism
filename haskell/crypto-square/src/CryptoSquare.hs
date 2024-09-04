{-# OPTIONS_GHC -Wall #-}

-- https://exercism.org/tracks/haskell/exercises/crypto-square

module CryptoSquare (encode) where

import Data.Char

encode :: String -> String
encode xs
  | length (normalize xs) == 1 = normalize xs
  | otherwise = unwords $ chunks rows' rotatesStr
 where
  (cols, rows) = getColumnAndRows (length xs)
  (_, rows') = getColumnAndRows (length rotatesStr)
  splitStr = splitAndPad rows (normalize xs)
  (rotatesStr, _) = last (take cols (iterate splitList ("", splitStr)))

normalize :: String -> String
normalize xs = removeWhitespace . removePunc $ lower xs
 where
  lower = map toLower
  removePunc ys = [x | x <- ys, x `notElem` "@%,.?!-:;\"\'"]
  removeWhitespace = concat . words

getColumnAndRows :: Int -> (Int, Int)
getColumnAndRows msgSize = getColRow (1, 1)
 where
  isValidColRow (c, r) = c * r >= msgSize && c >= r && c - r <= 1
  getColRow (c, r)
    | isValidColRow (c, r) = (c, r)
    | c == r = getColRow (c + 1, r)
    | otherwise = getColRow (c, r + 1)

splitAndPad :: Int -> String -> [String]
splitAndPad y xs = start ++ [paddedEnd]
 where
  splitChunks = chunks y xs
  start = init splitChunks
  end = last splitChunks
  lenEnd = length end
  paddedEnd = if lenEnd < y then end ++ replicate (y - lenEnd) ' ' else end

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  let (ys, zs) = splitAt n xs
   in ys : chunks n zs

splitList :: (String, [String]) -> (String, [String])
splitList (xs, ys) = (xs ++ items, remainder)
 where
  (items, remainder) = splitList' ys

splitList' :: [String] -> (String, [String])
splitList' [] = ([], [])
splitList' xs = (thing1, thing2)
 where
  thing1 = concatMap (take 1) xs
  thing2 = map (drop 1) xs
