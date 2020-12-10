{-# OPTIONS_GHC -Wall #-}
module Pidlisnyi02 where

-- Задача 1 -----------------------------------------
sumFr :: [Integer] -> Integer
sumFr xs  = foldr (+) 0 xs
  
-- Задача 2 ----------------------------------------- 
factorial :: Integer -> Integer
factorial n = foldl (*) 1 [1..n]

-- Задача 3 -----------------------------------------
concatFr :: [Integer] -> [Integer] -> [Integer]
concatFr xs ys = foldr (:) ys xs

-- Задача 4 -----------------------------------------
sortInsert :: [Integer] -> [Integer]
sortInsert  = foldl myInsert []
myInsert :: [Integer] -> Integer -> [Integer]
myInsert [] x = [x]
myInsert xs v
  | v < (head xs) = v : (head xs) : (tail xs)
  | otherwise = (head xs) : myInsert (tail xs) v 

-- Задача 5 -----------------------------------------
map2 :: (a->b->c) -> [a] -> [b] -> [c]
map2 f xs ys  = if (null xs) || (null ys) then [] else
    (f (head xs) (head ys)) : map2 f (tail xs) (tail ys)

-- Задача 6 -----------------------------------------
expPart :: Integer -> Integer -> Double
expPart m n = sum ([fromIntegral (m ^ i) / fromIntegral (factorial i) | i <- [1..n]])

-- Задача 7 -----------------------------------------
triangle :: [Integer]
triangle = scanl1 (+) [1..]

-- Задача 8 -----------------------------------------
piramid :: [Integer]
piramid = scanl1 (+) [x*x | x <- [1..]]

-- Задача 9 -----------------------------------------
indexes :: [Int] -> [Int] -> [Int]
indexes = undefined
-- indexes xs ys = if (null ys) && (null xs) then [0] else
--   if (null ys) && (not (null xs)) then [] else
--     if (take (lng xs) ys) == xs then 0 : indexes xs (drop (lng xs) ys) else
--       indexes xs (tail ys)

-- lng :: [a] -> Int 
-- lng []     = 0 
-- lng (_:xs) = 1 + lng xs