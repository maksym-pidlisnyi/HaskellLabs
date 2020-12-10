{-# OPTIONS_GHC -Wall #-}
module Pidlisnyi10 where

data BinTree a = EmptyB 
                | Node a (BinTree a) (BinTree a)
                   deriving (Show, Eq)
data Tree23 a  = Leaf a   
               | Node2 (Tree23 a) a (Tree23 a) 
               | Node3 (Tree23 a) a (Tree23 a) a (Tree23 a)
               | Empty23     -- порожнє 2-3-дерево!!!
                   deriving (Eq, Show)

-- Задача 1 -----------------------------------------			   
isSearch :: (Ord a) => BinTree a -> Bool
isSearch EmptyB = True
isSearch (Node p s1 s2) = (isCorrectyStructured s1 p (<)) &&
 (isCorrectyStructured s2 p (>)) && isSearch s1 && isSearch s2 where
        isCorrectyStructured EmptyB _ _ = True
        isCorrectyStructured (Node h ns1 ns2) x f = (f h x) &&
         (isCorrectyStructured ns1 x f) && (isCorrectyStructured ns2 x f)

-- Задача 2-----------------------------------------
elemSearch :: (Ord a) => BinTree a -> a -> Bool
elemSearch EmptyB _ = False
elemSearch (Node p s1 s2) v
        | v == p = True
        | v > p = elemSearch s2 v
        | otherwise = elemSearch s1 v

-- Задача 3 -----------------------------------------
insSearch :: (Ord a) => BinTree a -> a -> BinTree a 
insSearch EmptyB v = Node v EmptyB EmptyB
insSearch (Node p s1 s2) v
        | v == p = (Node p s1 s2)
        | v > p = (Node p s1 (insSearch s2 v))
        | otherwise = (Node p (insSearch s1 v) s2)

-- Задача 4 -----------------------------------------
delSearch :: (Ord a) => BinTree a -> a -> BinTree a 
delSearch EmptyB v = Node v EmptyB EmptyB
delSearch (Node p s1 s2) v
        | v == p = mergeTrees s1 s2
        | v > p = (Node p s1 (delSearch s2 v))
        | otherwise = (Node p (delSearch s1 v) s2) where
                mergeTrees tr EmptyB = tr
                mergeTrees EmptyB tr = tr
                mergeTrees (Node h ns1 ns2) tr = Node h ns1 (mergeTrees ns2 tr)

-- Задача 5 -----------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList xs = treeToList (foldl insSearch EmptyB xs)
treeToList :: (Ord a) => BinTree a -> [a]
treeToList EmptyB = []
treeToList (Node p s1 s2) = treeToList s1 ++ p : treeToList s2

-- Задача 6-----------------------------------------
isTree23  :: (Ord a) => Tree23 a -> Bool 
isTree23 Empty23 = True
isTree23 (Leaf _) = True
isTree23 (Node2 s1 p s2) = if ((p == minTr s2) && (p >= maxTr s1)) then True else False
isTree23 (Node3 s1 p1 s2 p2 s3) = if ((p1 == minTr s2) && (p1 >= maxTr s1) &&
        (p2 == minTr s3) && (p2 >= maxTr s2)) then True else False
minTr :: (Ord a) => Tree23 a -> a
minTr (Leaf l) = l 
minTr (Node2 Empty23 x _) = x
minTr (Node2 x _ _) = minTr x
minTr (Node3 Empty23 x _ _ _) = x 
minTr (Node3 x _ _ _ _) = minTr x
minTr Empty23 = undefined
maxTr :: (Ord a) => Tree23 a -> a
maxTr (Leaf l) = l 
maxTr (Node2 _ x Empty23) = x
maxTr (Node2 _ _ x) = maxTr x
maxTr (Node3 _ _ _ x Empty23) = x 
maxTr (Node3 _ _ _ _ x) = maxTr x
maxTr Empty23 = undefined

-- Задача 7-----------------------------------------
elemTree23 :: (Ord a) => Tree23 a -> a -> Bool
elemTree23 (Empty23) _ = False
elemTree23 (Leaf l) v = if (l == v) then True else False
elemTree23 (Node2 s1 p s2) v = 
        case compare v p of
                EQ -> True
                GT -> elemTree23 s2 v
                LT -> elemTree23 s1 v
elemTree23 (Node3 s1 p1 s2 p2 s3) v = 
        case (compare v p1, compare v p2) of
                (EQ, _) -> True
                (_, EQ) -> True
                (_, GT) -> elemTree23 s3 v
                (LT, _) -> elemTree23 s1 v
                (_, LT) -> elemTree23 s2 v

-- Задача 8-----------------------------------------
eqTree23 :: (Ord a) => Tree23 a -> Tree23 a -> Bool
eqTree23 t1 t2 = (tree23ToList t1) == (tree23ToList t2)
tree23ToList :: (Ord a) => Tree23 a -> [a]
tree23ToList Empty23 = []
tree23ToList (Leaf l) = [l]
tree23ToList (Node2 s1 _ s2) = tree23ToList s1 ++ tree23ToList s2
tree23ToList (Node3 s1 _ s2 _ s3) = tree23ToList s1 ++ tree23ToList s2 ++ tree23ToList s3

-- Задача 9-----------------------------------------
insTree23 :: (Ord a) => Tree23 a -> a -> Tree23 a
insTree23 (Empty23) v = (Leaf v)
insTree23 (Leaf l) v =
        case compare l v of
                EQ -> (Leaf l)
                LT -> Node2 (Leaf l) v (Leaf v)
                GT -> Node2 (Leaf v) l (Leaf l)
insTree23 (Node2 s1 p s2) v = 
        case compare p v of 
                EQ -> (Node2 s1 v s2)
                LT -> insertTree2 s1 p (insTree23 s2 v)
                GT -> insertTree2 (insTree23 s1 v) p s2 
insTree23 (Node3 s1 p1 s2 p2 s3) v = 
        case (compare v p1, compare v p2) of
                (EQ, _) -> Node3 s1 p1 s2 p2 s3
                (_, EQ) -> Node3 s1 p1 s2 p2 s3
                (_, GT) -> insertTree3 s1 p1 s2 p2 (insTree23 s3 v)
                (LT, _) -> insertTree3 (insTree23 s1 v) p1 s2 p2 s3
                (_, LT) -> insertTree3 s1 p1 (insTree23 s2 v) p2 s3

insertTree2 :: (Ord a) => Tree23 a -> a -> Tree23 a -> Tree23 a 
insertTree2 (Leaf l1) p (Leaf l2) = Node2 (Leaf l1) p (Leaf l2)
insertTree2 (Node2 s1 p1 s2) p2 s3 = Node3 s1 p1 s2 p2 s3
insertTree2 s1 p1 (Node2 s2 p2 s3) = Node3 s1 p1 s2 p2 s3
insertTree2 s1 p s2 = Node2 s1 p s2
insertTree3 :: (Ord a) => Tree23 a -> a -> Tree23 a -> a -> Tree23 a -> Tree23 a
insertTree3 (Leaf l1) p1 (Leaf l2) p2 (Leaf l3) = Node3 (Leaf l1) p1 (Leaf l2) p2 (Leaf l3)
insertTree3 s1 p1 s2 p2 (Node2 s3 p3 s4) = Node2 (Node2 s1 p1 s2) p2 (Node2 s3 p3 s4)
insertTree3 s1 p1 (Node2 s2 p2 s3) p3 s4 = Node2 (Node2 s1 p1 s2) p2 (Node2 s3 p3 s4)
insertTree3 (Node2 s1 p1 s2) p2 s3 p3 s4 = Node2 (Node2 s1 p1 s2) p2 (Node2 s3 p3 s4)
insertTree3 s1 p1 s2 p2 s3 = Node3 s1 p1 s2 p2 s3

-- isTerminal tr = True <=> якщо сини вузла tr - листки !!
isTerminal :: (Ord a) => Tree23 a -> Bool
isTerminal (Node2 (Leaf _) _ _)     = True 
isTerminal (Node3 (Leaf _) _ _ _ _) = True
isTerminal _  = False

-- Результат вставки вузла в 2-3-дерево, 
--   корінь якого - вузол вида Node2 або Node3 є об`єкт із (Tree23 a, Maybe (a, Tree23 a))
--   : (a, Nothing) - результат вставки - одне 2-3-дерево a 
--   : (a, Just (w, b)) - результат вставки два 2-3-дерева a i b (w - найменше значення в b)
--  insert v tr - додає значення v в довільне дерево tr
insert :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insert v tr | isTerminal tr = insTerm v tr
            | otherwise = insNode v tr

-- insTerm v tr - додається значення v в дерево tr з конем - термінальний вузол 
insTerm :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insTerm = undefined

-- insNode v tr - додає значення v в дерево tr з корнем - нетермінальний вузол 
insNode :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insNode = undefined

---  Бінарні дерева 
bt1, bt2 ::  BinTree Int
bt1 = Node 9 (Node 4 EmptyB 
                     (Node 8 EmptyB EmptyB))
             (Node 20 (Node 10 EmptyB EmptyB) 
                      EmptyB)
bt2 = Node 9 (Node 4 EmptyB 
                     (Node 8 (Node 6 EmptyB EmptyB)
                             EmptyB))
             (Node 20 (Node 10 EmptyB EmptyB) 
                       EmptyB)

---- 2-3-дерева
tr1, tr2, tr3, tr4,tr5 :: Tree23 Int
tr1 =  Node2 (Node2 (Node2 (Leaf 0) 1 (Leaf 1)) 
                     2
                    (Node2 (Leaf 2) 3 (Leaf 3)))
              4
             (Node2 (Node2 (Leaf 4) 5 (Leaf 5)) 
                     6
                    (Node2 (Leaf 6) 7 (Leaf 7)))
tr2 =  Node3 (Node2 (Leaf 0) 1 (Leaf 1))
              2
             (Node3 (Leaf 2) 3 (Leaf 3) 4 (Leaf 4))
              5
             (Node3 (Leaf 5) 6 (Leaf 6) 7 (Leaf 7))

tr3 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node2 (Leaf 16) 19 (Leaf 19))

tr4 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))

tr5 = Node2 (Node2 (Node2 (Leaf 2) 5 (Leaf 5))
                    7
                   (Node2 (Leaf 7) 8 (Leaf 8)) 
            )
            10
            (Node2 (Node2 (Leaf 10) 12 (Leaf 12))
                   16
                   (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))
            )