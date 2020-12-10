{-# OPTIONS_GHC -Wall #-}
module Pidlisnyi06 where
import Data.List

newtype Poly a = P [a]

-- Задача 1 -----------------------------------------
x :: Num a => Poly a
x = P [0, 1]

-- Задача 2 ----------------------------------------
instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P a) (P b) = 
       (dropWhile (isZero) (reverse a)) == (dropWhile (isZero) (reverse b))
       where
           isZero n = n == 0
 
-- Задача 3 -----------------------------------------
instance (Num a, Eq a, Show a) => Show (Poly a) where
    -- show (P []) = "0"
    show (P a)
        | null a = "0"
        | (P a) == (P [0]) = "0"
        | otherwise = concat . reverse . (intersperse " + ") .
         (filter (\el -> el /= "")) . (map (\(val,coef) -> ( 
                    if coef == (fromInteger 0) then "" else (
                        if coef == (fromInteger 1) && val > 0 then "" else (
                            if coef == (fromInteger (-1)) && val > 0 then "-" else
                                show coef
                        )
                    ) ++ (
                        if val > 1 then "x^" ++ show val else
                            if val == 1 then "x" else ""
                    )
                )
            )
        ) . (zip [0..((length a)-1)]) $ a

-- Задача 4 -----------------------------------------
plus :: Num a => Poly a -> Poly a -> Poly a
plus (P a) (P b) = P (listSum a b)
listSum :: Num a => [a] -> [a] -> [a]
-- listSum xs ys = if (null xs) && (null ys) then [] else
--        if (null xs) && (not (null ys)) then (0 + (head ys)) : listSum [] (tail ys) else 
--              if (null ys) && (not (null xs)) then ((head xs) + 0) : listSum (tail xs) [] else
--                     ((head xs) + (head ys)) : listSum (tail xs) (tail ys)
listSum ys [] = ys
listSum [] ys = ys
listSum (y:ys) (z:zs) = (y + z) : listSum ys zs

-- Задача 5 -----------------------------------------
times :: Num a => Poly a -> Poly a -> Poly a
times (P []) _ = P []
times (P (y:ys)) (P zs) = P (map (y *) zs) + ((P ys) * (P (0:zs)))

-- Задача 6 -----------------------------------------
instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P a) = P (map negate a)
    fromInteger n = P [fromInteger n]
    -- Розумних означень не існує
    abs    = undefined
    signum = undefined

-- Задача 7 -----------------------------------------
applyP :: Num a => Poly a -> a -> a
applyP (P []) _ = 0
applyP (P (y:ys)) p = y + (p * applyP (P ys) p)

-- Задача 8 -----------------------------------------
class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n a
        | n < 1 = a
        | otherwise = nderiv (n - 1) (deriv a)

-- Задача 9 -----------------------------------------
instance Num a => Differentiable (Poly a) where
    deriv (P []) = P []
    deriv (P (_:a)) = P a + (x * deriv (P a)) 