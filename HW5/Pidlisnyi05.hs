{-# OPTIONS_GHC -Wall #-}
module Pidlinyi05 where
-- import Data.List

type Graph = [[Int]]

-- Задача 1 ------------------------------------------
lucky ::   Int ->  [String]
lucky n = toString $ [x| x <- [(10^(2*n-1)+1)..(10^(2*n))],
 (sumDigits $ x `div` 10^n) == (sumDigits $ x `mod` 10^n)]
 
sumDigits :: Int -> Int
sumDigits 0 = 0
sumDigits x = (x `mod` 10) + sumDigits (x `div` 10)
toString:: [Int] -> [String]
toString xs = if null xs then [] else map show xs

-- Задача 2 -----------------------------------------  
queens ::  Int -> [[Int]]
queens n = map reverse (create n) where
    create 0 = [[]]
    create m = [x:xs| xs <- create (m-1), x <- [1..n], isSafe x xs]
    isSafe q xs = not (elem q xs || elem q (diagonals 1 xs))
diagonals :: Int -> [Int] -> [Int]
diagonals _ [] = []
diagonals n (x:xs) = (n + x) : (x-n) : diagonals (n+1) xs
   
-- Задача 3 -----------------------------------------
maxLen ::  [Int] -> Int
maxLen xs = length (foldl1 maxLenghtList $ allLists xs)

maxLenghtList :: [Int] -> [Int] -> [Int]
maxLenghtList xs ys
 | length xs > length ys = xs
 | otherwise = ys
sortInsert :: [Int] -> [Int]
sortInsert  = foldl myInsert []
myInsert :: [Int] -> Int -> [Int]
myInsert [] x = [x]
myInsert xs v
  | v < (head xs) = v : (head xs) : (tail xs)
  | otherwise = (head xs) : myInsert (tail xs) v 
set :: Eq a => [a] -> [a] 
set xs = if null xs then [] else (head xs) : set (filter ((head xs) /=) (tail xs))
sublists :: [Int] -> [[Int]]
sublists []  = [[]]
sublists (x:xs) = let yss = sublists xs
                         in  yss ++ map (x:) yss 
allLists :: [Int] -> [[Int]]
allLists xs = filter check (sublists xs) where
    check ys = (sortInsert (set ys)) == ys
   
-- Задача 4 -----------------------------------------
maxSeq ::  [Int] ->  [Int]
maxSeq xs = foldl1 maxLenghtList $ allLists xs

-- Задача 5 -----------------------------------------
allMaxSeq ::  [Int] -> [[Int]]
allMaxSeq xs = [ys| ys <- (allLists xs), (length ys) == (maxLen xs)]

-- Задача 6 -----------------------------------------
genExpr ::  Int -> Int -> [String]
genExpr a b = let
            ops = (length $ show a) - 1
            expr n _ | n <= 0 = [[]]
            expr 1 xs = map (:[]) xs
            expr n xs = (:) <$> xs <*> expr (n-1) xs
            merge [] opers = opers
            merge nums opers = if length nums == (ops + 1) then
                 (head nums) : ' ' : merge (tail nums) opers else
                      (head nums) : ' ' : (head opers) : ' ' : merge (tail nums) (tail opers)
        in map (\z -> convertExpr (filter (/=' ') z) (ops + ops + 1)) $ 
        filter (\y -> countExpr y == b) $ map (\x -> merge (show a) x) (expr ops "+-*")

convertExpr :: String -> Int -> String
convertExpr [] _ = []
convertExpr (x:xs) n = 
    if length (x:xs) == n
        then x : convertExpr xs n
        else (head xs) : x : convertExpr (tail xs) n
countExpr :: String -> Int
countExpr expr = let 
    convertEl (x:x':xs) "+" = (x + x') : xs
    convertEl (x:x':xs) "-" = (x' - x) : xs
    convertEl (x:x':xs) "*" = (x * x') : xs
    convertEl xs nums = read nums : xs 
    in head (foldl convertEl [] (words expr))
-- insert :: a -> [a] -> [[a]]
-- insert v []    = [[v]] 
-- insert v (y:ys)= (v:y:ys):(map (y:)(insert v ys))
-- merge :: [String] -> String -> [String]
-- merge nums ops = if null ops then nums else
--     merge [expr| xs <- map (insert $ head ops) nums, expr <- xs] ops

-- permit :: Int -> String -> [String]
-- permit 0 _  = [[]]
-- permit n st = [x:y | y <- permit (n-1) st, x <- st]
-- createOperations :: String -> [String]
-- createOperations nums = permit ((length nums) - 1) "+-*"
-- addOperations :: String -> String -> [String]
-- addOperations x [] = [x]
-- addOperations n (x:xs) = merge (insert x n) xs
-- allExpr :: Int -> Int -> [String]
-- allExpr a b = set ([expr| opsAll <- createOperations (show a),
--  expr <- addOperations (show a) opsAll, (countExpr expr) == b])

-- Задача 7 -----------------------------------------
genExprBracket ::  Int -> Int -> [String]
genExprBracket = undefined

-- Задача 8 -----------------------------------------
topolSortAll :: Graph -> [[Int]]
topolSortAll = undefined

--------------------------------------------
gr1 :: Graph 
gr1 = [[1,2,3], [], [3,4], [4],[]]