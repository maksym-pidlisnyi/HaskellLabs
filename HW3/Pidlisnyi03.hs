{-# OPTIONS_GHC -Wall #-}
module Pidlisnyi03 where

-- ��� - ������ ������ ������� - ���������� ���� '0' ..'9'
type Code = String

-- ���� ��� (Move) ���� ����������� Move �������������� ������ (Code) � ��� �����:  
--    ������� "����" � "����"  � ����������-����� �� ��������� �� ����-����� 
data Move = Move Code Int Int
          deriving (Show, Eq)

-- ������ 1 -----------------------------------------
exactMatches :: Code -> Code -> Int
exactMatches xs ys = if (null xs) || (null ys) then 0 else
    if ((head xs) == (head ys)) then 1 + (exactMatches (tail xs) (tail ys)) else
        0 + (exactMatches (tail xs) (tail ys))

-- ������ 2 -----------------------------------------
countDigits :: Code -> [Int]
countDigits cd = map f ['0'..'9'] where f x = length . filter (== x) $ cd

-- ������ 3 ----------------------------------------- 
matches :: Code -> Code -> Int
matches xs ys = foldr1 (+) (minFromAll (countDigits xs) (countDigits ys))

minFromAll :: [Int] -> [Int] -> [Int]
minFromAll xs ys = if (null xs) || (null ys) then [] else
     (min (head xs) (head ys)) : minFromAll (tail xs) (tail ys)
 
-- ������ 4 -----------------------------------------
getMove :: Code -> Code -> Move
getMove cd att = Move att bulls (overall - bulls) where
    overall = matches cd att
    bulls = exactMatches cd att

-- ������ 5 -----------------------------------------
isConsistent :: Move -> Code -> Bool
isConsistent (Move att bulls overall) cd = (getMove cd att) == (Move att bulls overall)

-- ������ 6 -----------------------------------------
filterCodes :: Move -> [Code] -> [Code]
filterCodes mv cdx = filter (isConsistent mv) cdx

-- ������ 7 -----------------------------------------
allCodes :: Int -> [Code]
allCodes n = extand [] n
 
extand :: [Code] -> Int -> [Code]
extand cdx d = if null cdx then extand (addNums []) (d-1) else
     if d == 0 then cdx else
         extand (concatMap (\base -> addNums base) cdx) (d-1)

addNums :: String -> [String]
addNums base = map (\c -> base ++ [c]) ['0'..'9']
   
-- ������ 8 -----------------------------------------
solve :: Code -> [Move]
solve cd = moves codes where
    moves xs = if null xs then [] else
         (getMove cd (head xs)) : (moves (filterCodes (getMove cd (head xs)) (tail xs)))
    codes = allCodes (length cd)     