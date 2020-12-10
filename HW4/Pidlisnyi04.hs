{-# OPTIONS_GHC -Wall #-}
module Pidlisnyi04 where

type Graph  = [[Int]]

-- Задача 1 ------------------------------------
isGraph :: Graph -> Bool 
isGraph [] = True
isGraph [[_]] = False
isGraph (v:gr) = (hasAllDifferent v) && (isGraph gr)
hasAllDifferent :: [Int] -> Bool
hasAllDifferent [] = True
hasAllDifferent [_] = True
hasAllDifferent (x:xs) = (not (x `elem` xs)) && (hasAllDifferent xs)

-- Задача 2 ------------------------------------
isTournament :: Graph -> Bool 
-- isTournament gr = if (isGraph gr) then (length $ edges gr) == (2 * length gr) else False
isTournament gr = (length $ edges gr) == (2 * length gr)
nodes :: Graph -> [Int]
nodes g = [0..(length g - 1)]
edges :: Graph -> [(Int,Int)]
edges g = [(x,y) | x<-nodes g, y <- g!!x]   

-- Задача 3 ------------------------------------
isTransitive :: Graph -> Bool
-- isTransitive gr = if (isGraph gr) then and [elem (fst x, snd y) (edges gr) | x <- (edges gr), y <- (edges gr), snd x == fst y] else False
isTransitive gr = and [elem (fst x, snd y) (edges gr)
 | x <- (edges gr), y <- (edges gr), snd x == fst y]

-- Задача 4 ------------------------------------
buildTransitive :: Graph -> Graph 
buildTransitive = undefined

-- Задача 5 ------------------------------------
longWay :: Graph -> Int -> Int -> Maybe [Int] 
longWay gr a b = if null (waysAB gr a b) then Nothing else
     if a == b then Nothing else
          Just (reverse (head $ waysAB gr a b))

waysAB :: Graph -> Int -> Int -> [[Int]]
waysAB gr a b = [way1 | waysAll <- allWays gr a, way1 <- waysAll, (head way1) == b]
allWays :: Graph -> Int -> [[[Int]]]
allWays gr v = until condW (stepW gr) [[[v]]]
condW :: ([[[Int]]]) -> Bool
condW wss = null ( head wss)
 -- condW = null . head
stepW :: Graph -> [[[Int]]] -> [[[Int]]]
stepW gr wss@(wsn:_) =
  [t:w | w@(x:xs) <- wsn, notElem x xs, t<- gr!!x] : wss
stepW _ []  = error "allWays:stepW"

-- Задача 6 ------------------------------------
gamiltonWay :: Graph -> Maybe [Int]
gamiltonWay = undefined

-- Задача 7 ------------------------------------
isAcyclic :: Graph -> Bool 
isAcyclic = undefined
-- isAcyclic [] = True
-- isAcyclic [[]] = True
-- isAcyclic gr = 

-- Задача 8 ------------------------------------
topolSort :: Graph -> Maybe [Int] 
topolSort = undefined

-- Задача 9------------------------------------
isTopolSort :: Graph -> [Int] -> Bool 
isTopolSort = undefined

---------------------Тестові дані - Графи -------

gr1, gr2, gr3, gr4:: Graph
gr1 = [[1,2,3],[2,3],[3,4],[4],[]]
gr2 = [[3,4],[0,3],[0,1,4],[2,4],[1]]
gr3 = [[1],[2],[3],[1],[0,3]]
gr4 = [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[0,1,2,3]]