{-# OPTIONS_GHC -Wall #-}
module Pidlisnyi11 where

import Data.Char(isLower, isLetter)
import Data.Maybe

data BExp = Bvalue Bool | Bvar Char | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)
type Env = [(Char, Bool)]

type NodeId = Int
type BDDNode =  (NodeId, (Char, NodeId, NodeId))
type BDD = (NodeId, [BDDNode])

-- ������ 1 -----------------------------------------
checkSat :: BDD -> Env -> Bool
checkSat (iD, ns) env
    | iD == 0 = False
    | iD == 1 = True
    | findItem i env = checkSat (right, ns) env
    | otherwise = checkSat (left, ns) env where 
        (i, left, right) = findItem iD ns
findItem :: Eq a => a -> [(a, b)] -> b
findItem a lst = fromJust $ lookup a lst

-- ������ 2 -----------------------------------------
sat :: BDD -> [[(Char, Bool)]]
sat (iD, ns)
    | iD == 0 = []
    | iD == 1 = [[]]
    | otherwise = satFalse ++ satTrue where
        (i, left, right) = findItem iD ns
        satFalse = toList (i, False) (sat (left, ns))
        satTrue = toList (i, True) (sat (right, ns))
        toList x xs = [(x : y) | y <- xs]

-- ������ 3 -----------------------------------------
simplify :: BExp -> BExp
simplify (Or (Bvalue b) (Bvalue b')) = Bvalue (b || b')
simplify (And (Bvalue b) (Bvalue b')) = Bvalue (b && b') 
simplify (Not (Bvalue b)) = Bvalue (not b)
simplify (Or a b) = Or (simplify a) (simplify b)
simplify (And a b) = And (simplify a) (simplify b)
simplify (Not b) = Not $ simplify b
simplify b = b

-- simplify :: BExp -> BExp
-- simplify (Not (Bvalue True))                = Bvalue False
-- simplify (Not (Bvalue False))               = Bvalue True
-- simplify (And (Bvalue True) (Bvalue True))  = Bvalue True
-- simplify (And (Bvalue _) (Bvalue _))        = Bvalue False
-- simplify (Or (Bvalue False) (Bvalue False)) = Bvalue False
-- simplify (Or (Bvalue _) (Bvalue _))         = Bvalue True
-- simplify be                                 = be

-- ������ 4 -----------------------------------------
restrict :: BExp -> Char -> Bool -> BExp
restrict (Bvar i1) i b
    | i1 == i = Bvalue b
    | otherwise = Bvar i1
restrict (Bvalue b) _ _ = Bvalue b
restrict (Or x y) i b = simplify $ Or (restrict x i b) (restrict y i b)
restrict (And x y) i b = simplify $ And (restrict x i b) (restrict y i b)
restrict (Not x) i b = simplify $ Not (restrict x i b)

-- ������ 5 -----------------------------------------
-- ����������: ����� ����� (�����) � �������� ����� (BExp) �"��������� 
--    ����� ���� ��� � ������ ������ (Char); ���� ����� ��������
buildBDD :: BExp -> [Char] -> BDD
buildBDD e xs = buildBDD' e 2 xs 

buildBDD' :: BExp -> NodeId -> [Char] -> BDD
buildBDD' (Bvalue b) _ [] = if b then (1, []) else (0, [])
buildBDD' _ iD [] = (iD, [])
buildBDD' b iD (x: xs) = (iD, (iD, (x, falseN, trueN)) : falseNs ++ trueNs) where
    (falseN, falseNs) = buildBDD' (restrict b x False) (iD * 2) xs
    (trueN, trueNs) = buildBDD' (restrict b x True) (iD * 2 + 1) xs

-- ������ 6 -----------------------------------------
-- ����������: ����� ����� (�����) � �������� ����� (BExp) �"��������� 
--    ����� ���� ��� � ������ ������ (Char); ���� ����� ��������  
buildROBDD :: BExp -> [Char] -> BDD
buildROBDD b xs = buildROBDD' b 2 xs []
buildROBDD' :: BExp -> NodeId -> [Char] -> [BDDNode] -> BDD
buildROBDD' (Bvalue b) _ _ xs = if b then (1, xs) else (0, xs)
buildROBDD' b iD (x : xs) ns = if falseN == trueN then (falseN, falseNs) else
    maybe (iD, (iD, (x, falseN, trueN)) : trueNs) (\n -> (n, trueNs)) (reverseFindItem (x, falseN, trueN) trueNs) where
        (falseN, falseNs) = buildROBDD' (restrict b x False) (iD * 2) xs ns
        (trueN, trueNs) = buildROBDD' (restrict b x True) (iD * 2 + 1) xs falseNs
buildROBDD' _ iD [] _ = (iD, [])
reverseFindItem :: Eq a => a -> [(b, a)] -> Maybe b
reverseFindItem v xs = lookup v [(y,z)| (z, y) <- xs]

-- ������ 7 -----------------------------------------
fullBexp :: String -> Maybe BExp 
fullBexp s = case bexp s of
    Just (b, str) -> if null str then Just b else Nothing
    _ -> Nothing

bexp :: String -> Maybe (BExp,String)
bexp s = case bcon s of
    Just (b, str) -> manyCon (b, str)
    _ -> Nothing

bcon :: String -> Maybe (BExp,String)
bcon s = case bdis s of 
    Just (b, str) -> manyDis (b, str)
    _ -> Nothing

manyCon :: (BExp,String) -> Maybe (BExp,String)
manyCon (b, []) = Just (b, "")
manyCon (b, (x : xs)) = case x of
    '|' -> case bcon xs of 
        Just (be, str) -> manyCon (Or b be, str)
        _ -> Nothing
    _ -> Just (b, (x : xs))

bdis :: String -> Maybe (BExp,String)
bdis [] = Nothing
bdis (x : xs)
    | bsym x = Just (Bvar x, xs)
    | (x == '(' && elem ')' xs) = case bexp xs of
        Just (b, s) -> if head s == ')' then Just (b, tail s) else Nothing
        _ -> Nothing
    | x == '!' = case bdis xs of
        Just (b, s) -> Just (Not b, s)
        _ -> Nothing
    | x == 'T' = Just (Bvalue True, xs)
    | x == 'F' = Just (Bvalue False, xs)
    | otherwise = Nothing

bsym :: Char -> Bool
bsym c =  (isLower c) && (isLetter c)

manyDis :: (BExp,String) -> Maybe (BExp,String)
manyDis (b, []) = Just (b, "")
manyDis (b, (x : xs)) = case x of
    '&' -> case bdis xs of 
        Just (be, str) -> manyDis (And b be, str)
        _ -> Nothing
    _ -> Just (b, (x : xs))

------------------------------------------------------
-- �������� ��� ����������..
bs1, bs2, bs3, bs4, bs5, bs6, bs7, bs8, bs9 :: String
bs1 = "F"
bs2 = "!(x&(F|y))"
bs3 = "u&T"
bs4 = "d&(x|!y)"
bs5 = "!(d&(x|!y))"
bs6 = "u&x|y&z" 
bs7 = "!y|(x|!e)"
bs8 = "u|!u"
bs9 = "z&(y|!y&x)"

b1, b2, b3, b4, b5, b6, b7, b8, b9 :: BExp
b1 = Bvalue False
b2 = Not (And (Bvar 'x') (Or (Bvalue False) (Bvar 'y')))
b3 = And (Bvar 'u') (Bvalue True)
b4 = And (Bvar 'd') (Or (Bvar 'x') (Not (Bvar 'y')))
b5 = Not (And (Bvar 'd') (Or (Bvar 'x') (Not (Bvar 'y'))))
b6 = Or (And (Bvar 'u') (Bvar 'x')) (And (Bvar 'y') (Bvar 'z'))
b7 = Or (Not (Bvar 'y')) (Or (Bvar 'x') (Not (Bvar 'e')))
b8 = Or (Bvar 'u') (Not (Bvar 'u'))
b9 = And (Bvar 'z') (Or (Bvar 'y') (And (Not (Bvar 'y')) (Bvar 'x')))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8, bdd9 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(2,('x',4,5)),(4,('y',1,1)),(5,('y',1,0))])
bdd3 = (5,[(5,('u',0,1))])
bdd4 = (2,[(2,('x',4,5)),(4,('y',8,9)),(8,('d',0,1)),(9,('d',0,0)),
           (5,('y',10,11)),(10,('d',0,1)),(11,('d',0,1))])
bdd5 = (3,[(4,('y',8,9)),(3,('x',4,5)),(8,('d',1,0)),(9,('d',1,1)),
           (5,('y',10,11)),(10,('d',1,0)),(11,('d',1,0))])
bdd6 = (2,[(2,('u',4,5)),(4,('x',8,9)),(8,('y',16,17)),(16,('z',0,0)),
           (17,('z',0,1)),(9,('y',18,19)),(18,('z',0,0)),(19,('z',0,1)),
           (5,('x',10,11)),(10,('y',20,21)),(20,('z',0,0)),(21,('z',0,1)),
           (11,('y',22,23)),(22,('z',1,1)),(23,('z',1,1))])
bdd7 = (6,[(6,('x',4,5)),(4,('y',8,9)),(8,('e',1,1)),(9,('e',1,0)),
           (5,('y',10,11)),(10,('e',1,1)),(11,('e',1,1))])
bdd8 = (2,[(2,('u',1,1))])
bdd9 = (2,[(2,('x',4,5)),(4,('y',8,9)),(8,('z',0,0)),(9,('z',0,1)),(5,('y',10,11)),(10,('z',0,1)),(11,('z',0,1))])



