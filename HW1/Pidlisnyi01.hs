{-# OPTIONS_GHC -Wall #-}
module Pidlisnyi01 where

-- Задача 1 -----------------------------------------
factorial :: Integer -> Integer
-- factorial n  = product [1..n]
-- factorial 0 = 1
-- factorial n = n ? factorial (n ? 1)
factorial n  = if n < 1 then 1 else n * factorial (n - 1)

-- Задача 2 -----------------------------------------
listSum :: [Int] -> [Int] -> [Int]
listSum xs ys = if (null xs) && (null ys) then [] else
       if (null xs) && (not (null ys)) then (0 + (head ys)) : listSum [] (tail ys) else 
             if (null ys) && (not (null xs)) then ((head xs) + 0) : listSum (tail xs) [] else
                    ((head xs) + (head ys)) : listSum (tail xs) (tail ys)
-- listSum x y | lx == ly  = zipWith (+) x y
--              | lx > ly   = zipWith (+) x (y ++ (replicate (lx - ly) 0)) 
--              | otherwise = zipWith (+) (x ++ (replicate (ly - lx) 0)) y
--              where lx = (length x)
--                    ly = (length y)

-- Задача 3 ----------------------------------------- 
oddEven :: [Int] -> [Int]  
oddEven xs = if null xs then xs else
      if (null (tail xs)) then head xs : oddEven [] else
            head (tail xs) : (head xs) : oddEven (tail (tail xs))

-- Задача 4 -----------------------------------------
position    ::  Int -> [Int] -> Int
position n xs = if notExist n xs then -1 else
      if (n == (head xs)) then 0 else (1 + position n (tail xs))
notExist :: Int -> [Int] -> Bool
notExist n xs = if null xs then True else
      if (n == (head xs)) then False else notExist n (tail xs)
                     
-- Задача 5 -----------------------------------------
set :: [Int] -> [Int] 
set xs = if null xs then [] else (head xs) : set (filter1 ((head xs) /=) (tail xs))
memb :: Int -> [Int] -> Bool
memb x ys = if null ys then False else if ((head ys) == x) then True else memb x (tail ys)
filter1 :: (Int -> Bool) -> [Int] -> [Int]
filter1 p xs = if null xs then [] else 
                 if  p (head xs) then (head xs) : filter1 p (tail xs) 
                                 else filter1 p (tail xs)

-- Задача 6 -----------------------------------------
union :: [Int] -> [Int] -> [Int]
union xs ys = set (xs +++ ys)
(+++) :: [Int] -> [Int] -> [Int]
(+++) [] ys = ys
(+++) (x:xs) ys = x : xs +++ ys

-- Задача 7 -----------------------------------------
intersection :: [Int] -> [Int] -> [Int]
intersection xs ys = if (null xs) || (null ys) then [] else
       set (if memb (head xs) ys then (head xs) : (intersection (tail xs) ys) else
       (intersection (tail xs) ys))

-- Задача 8 -----------------------------------------
factorialsM :: [Integer]
-- factorialsM = zipWith (*) [1..] (1 : factorialsM)
factorialsM = [factorial x | x <- [1..]]