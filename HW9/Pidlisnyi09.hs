{-# OPTIONS_GHC -Wall #-}
module Pidlisnyi09 where

data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Ord, Show)

-- ������ 1 -----------------------------------------
isPrefix :: String -> String -> Bool
isPrefix _ ""  = False
isPrefix "" _ = True
isPrefix (x:xs) (y:ys)
  | x == y = isPrefix xs ys
  | otherwise = False

-- ������ 2 -----------------------------------------
partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition x [] = ([], x, [])
partition [] x = ([], [], x)
partition x y = removePref x y 0 where
  removePref cx cy i
    | (i < length cx && i < length cy && cx!!i == cy!!i) = removePref cx cy (i+1)
    | otherwise = (take i cx, drop i cx, drop i cy)

-- ������ 3 -----------------------------------------
suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes xs = xs : suffixes (tail xs)

-- ������ 4 -----------------------------------------
isSubstring :: String -> String -> Bool
isSubstring cx cy 
  | cx == cy = True
  | otherwise = or $ map (isPrefix cx) (suffixes cy)

-- ������ 5 -----------------------------------------
findSubstrings :: String -> String -> [Int]
findSubstrings cx cy = [x| (suff, x) <- (zip (suffixes cy) [0..]), isPrefix cx suff]

-- ������ 6 -----------------------------------------
getIndices :: SuffixTree -> [Int]
getIndices (Leaf leaf) = [leaf]
getIndices (Node node) = sortInsert (concatMap (getIndices . snd) node)
sortInsert :: [Int] -> [Int]
sortInsert  = foldl myInsert []
myInsert :: [Int] -> Int -> [Int]
myInsert [] x = [x]
myInsert xs v
  | v < (head xs) = v : (head xs) : (tail xs)
  | otherwise = (head xs) : myInsert (tail xs) v 

-- ������ 7 ----------------------------------------- 
findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' "" (Leaf leaf) = [leaf]
findSubstrings' _  (Leaf _) = []
findSubstrings' cx (Node node) = concatMap getIndex node where
    getIndex (str1, str2)
      | null cx1  = getIndices str2
      | null cx2 = findSubstrings' cx1 str2
      | otherwise  = []
      where (_, cx1, cx2) = partition cx str1

-- ������ 8 -----------------------------------------
insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (cx, i) (Node []) = Node [(cx, (Leaf i))]
insert (cx, i) (Node (n:ns))
  | null str1 = Node (n:n1)
  | str1 /= cx1 = Node ((str1, (Node [(str3, cx2), (str2, (Leaf i))])) : ns)
  | str1 == cx1 = Node ((cx1, (insert (str2, i) cx2)) : ns) where
              (Node n1) = insert (cx, i) (Node ns)
              (str1, str2, str3) = partition cx cx1
              (cx1, cx2) = n
insert (_, _) (Leaf _) = (Node [])
insert (_, _) (Node (_:_)) = error "empty"

-- �� ������� ������
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

-- ������ 9 ----------------------------------------- 
longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring tr = takeLongest [s| s <- (treeToString tr), length (findSubstrings' s tr) >= 2] where
        treeToString (Node []) = [[]]
        treeToString (Leaf _) = [[]]
        treeToString (Node (n:ns)) = (map ((fst n)++) (treeToString (snd n))) ++ (treeToString (Node ns))
        takeLongest cx
          | null cx = []
          | otherwise = snd (maximum $ [(length c, c)| c <- cx])

------------------------------------------------------
-- �������� ����� � ��������� �����..

s1 :: String
s1 = "banana"

s2 :: String
s2  = "mississippi"

t1 :: SuffixTree
t1 = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]