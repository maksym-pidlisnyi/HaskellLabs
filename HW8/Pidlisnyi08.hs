{-# OPTIONS_GHC -Wall #-}
module Pidlisnyi08 where

import Data.Tree

-- Задача 1 -----------------------------------------			   
rank :: Tree a -> Int
rank tr = length (allSons tr)
allSons :: Tree a -> Forest a                                 --MB PEREIMINOVAT`
allSons (Node _ xs) = xs
-- counter :: Forest a -> Int
-- counter xs
--         | null xs = 0
--         | otherwise = 1 + counter (tail xs)

-- Задача 2-----------------------------------------
isBinomTree :: Ord a => Tree a -> Bool
isBinomTree (Node _ []) = True
isBinomTree tr = and $ [(reverse (toLengthList (forestToLists $ allSons tr)) == (0:[2^k| k <- [0..(rank tr)-1]]))] where
        toLengthList [] = []
        toLengthList (xs:yss) = (length xs) : toLengthList (yss)
forestToLists :: Forest a -> [[a]]
forestToLists [] = [[]]
forestToLists (Node x xs:ys) = (x : (dfsForest xs)) : (forestToLists ys)
dfsForest :: Forest a -> [a]
dfsForest [] = []
dfsForest (Node x []:ts) = x : dfsForest ts
dfsForest (Node x xs:ts) = x : (dfsForest xs) ++ dfsForest ts

-- Задача 3 -----------------------------------------
isBinomHeap :: Ord a => Forest a -> Bool
isBinomHeap f = and $ [isBinomTree tr| tr <- f]

-- Задача 4 -----------------------------------------
combineTrees :: Ord a => Tree a -> Tree a -> Tree a
-- combineTrees t t' = if (rank t == rank t') then combine t t' else eror "" where
combineTrees t t' = combine t t' where
        combine (Node x xs) (Node y ys) = if (x < y) then Node x ((Node y ys) : xs) else   -- combine trees with the same ranks
                Node y ((Node x xs) : ys)

-- Задача 5 -----------------------------------------
extractMin :: Ord a => Forest a -> a
extractMin f = minVal (dfsForest f)
minVal :: (Ord a) => [a] -> a 
minVal [] = error "empty list"
minVal [x] = x  
minVal (x:y:xs) = if x < y then minVal(x:xs) else minVal(y:xs)

-- Задача 6-----------------------------------------
mergeHeaps :: Ord a => Forest a -> Forest a -> Forest a
mergeHeaps h [] = h
mergeHeaps [] h = h
mergeHeaps (Node x xs:xss) (Node y ys:yss)
        | (rank $ Node x xs) > (rank $ Node y ys) = (Node y ys) : (mergeHeaps ((Node x xs) : xss) yss)
        | (rank $ Node x xs) < (rank $ Node y ys) = (Node x xs) : (mergeHeaps xss ((Node y ys) : yss))
        | otherwise = 
                if null xss then mergeHeaps (combineTrees (Node x xs) (Node y ys) : xss) yss else
                mergeHeaps xss (combineTrees (Node x xs) (Node y ys) : yss)

-- Задача 7-----------------------------------------
insert :: Ord a => a -> Forest a -> Forest a
insert v h = mergeHeaps [Node v []] h

-- Задача 8-----------------------------------------
deleteMin :: Ord a => Forest a -> Forest a
deleteMin [] = []
deleteMin h = mergeHeaps (delMin h (extractMin h)) (reverse $ restPart h (extractMin h))
delMin :: Ord a => Forest a -> a -> Forest a
delMin [] _ = []
delMin (Node x xs:xss) v
        | x == v = addLeftover xss
        | otherwise = (Node x xs) : (delMin xss v)  where 
                addLeftover f = if null f then [] else f
restPart :: Ord a => Forest a -> a -> Forest a
restPart [] _ = []
restPart (Node x xs:xss) v
        | null (Node x xs:xss) = []
        | x == v = xs
        | otherwise = restPart xss v 

-- Задача 9-----------------------------------------
binomSort :: Ord a => [a] -> [a]
binomSort [] = []
binomSort l = binSort (tail l) [(Node (head l) [])]
binSort :: Ord a => [a] -> Forest a -> [a]
binSort [] [] = []
binSort (x:xs) ys = binSort xs (insert x ys)
binSort [] xs = extractMin xs : binSort [] (deleteMin xs)

-- Задача 10 -----------------------------------------
toBinary :: Forest a -> [Int]
toBinary f = deleteZeros [binarify x (allRanks f)| x <- [length (dfsForest f), length (dfsForest f)-1..0]] where
        allRanks ys = if null ys then [] else
                rank (head ys) : allRanks (tail ys)
deleteZeros :: [Int] -> [Int]
deleteZeros xs = if null xs then xs else if ((head xs) == 0) then deleteZeros (tail xs) else xs
binarify :: Int -> [Int] -> Int
binarify _ [] = 0
binarify x xs
        | isPresent x xs = 1
        | otherwise = 0
isPresent :: Int -> [Int] -> Bool
isPresent _ [] = False
isPresent n (x:xs) 
        | (n - x) == 0 = True
        | otherwise = isPresent n xs
-----------------------------------------------------  
-- Приклади деяких дерев...

t1, t2, t3, t4, t5, t6, t7, t8 :: Tree Int
--  Зауваження: t7 - результат злиття t5 і t6

-- t1 .. t4 з'являються на Мал. 1...
t1 = Node 4  []
t2 = Node 1 [Node 5 []]
t3 = Node 2 [Node 8 [Node 9 []], Node 7 []]
t4 = Node 2 [Node 3 [Node 6 [Node 8 []], 
                     Node 10 []],
             Node 8 [Node 9 []],
             Node 7 []]

-- t5 і t6 зліва на Мал.2; t7 - справа на Мал.2
t5 = Node 4 [Node 6 [Node 8 []], 
                     Node 10 []]
t6 = Node 2 [Node 8 [Node 9 []], Node 7 []]
t7 = Node 2 [Node 4 [Node 6 [Node 8 []], Node 10 []],
             Node 8 [Node 9 []], 
             Node 7 []]

-- Додаткове дерево...
t8 = Node 12 [Node 16 []]

------------------------------------------------------
-- Приклади деяких куп...

h1, h2, h3, h4, h5, h6, h7 :: Forest Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 [Node 12 [Node 16 []],
              Node 5 []],
      Node 2 [Node 4 [Node 6 [Node 8 []],
                      Node 10 []],
              Node 8 [Node 9 []],
              Node 7 []]]

-- h3 показана на Мал.3...
h3 = [t1, t2, t4]

-- Дві додаткові купи використовуються далі. Вони зліва на Мал.4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 - результат злиття h4 і h5, справа на Мал.4(b)...
h6 = [Node 4 [],
      Node 1 [Node 4 [Node 6  [Node 8 []],
                      Node 10 []],
              Node 12 [Node 16 []],
              Node 5 []]]

-- h7 показана на Мал.5...
h7 = [Node 4 [Node 4 [Node 12 [Node 16 []],
                      Node 5 []],
              Node 6 [Node 8 []],
              Node 10 []]]  