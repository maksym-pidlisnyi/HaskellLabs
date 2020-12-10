{-# OPTIONS_GHC -Wall #-}
module Pidlisnyi07 where

import Data.Char(isSpace, isDigit, isLetter) 

type Name       = String
type Attributes = [(String, String)]
data XML        =  Text String | Element Name Attributes [XML] deriving (Eq, Show)

-- Задача 1 -----------------------------------------
spaces :: String -> String
spaces = dropWhile isSpace
  
-- Задача 2 ----------------------------------------- 
manyT, value, manyN :: String -> (String,String)
manyT s = (takeWhile sT s, dropWhile sT s)
sT :: Char -> Bool    -- from practice presentation
sT c = c `notElem` "<>"

value s = (takeWhile cV s, dropWhile cV s)
cV :: Char -> Bool    -- from practice presentation
cV c = c `notElem` "\"" 

manyN s = (takeWhile sN s, dropWhile sN s)
sN :: Char -> Bool    -- from practice presentation
sN c = isLetter c || isDigit c || c == '.' || c == '-'

-- Задача 3 -----------------------------------------
name, text, fullValue :: String ->  Maybe(String,String) 
name s
  | null s = Nothing
  | isLetter (head s) = Just (manyN s)
  | otherwise = Nothing

text s
  | null s = Nothing
  | sT (head s) = Just (manyT s)
  | otherwise = Nothing

fullValue [] = Nothing
fullValue (x:xs)
  | cV x || (count xs 0) < 0 = Nothing
  | otherwise = Just (fst $ value xs, drop 1 (snd $ value xs))
  where 
    count :: [Char] -> Int -> Int
    count [] _ = -1
    count (y:ys) n 
          | not $ cV y = n
          | otherwise = count ys (n+1)

-- Задача 4 -----------------------------------------
attrib :: String -> Maybe ((String,String),String) 
attrib s = case name (spaces s) of
            Just (attr, rest) -> case value rest of
                                  (eq, other) -> case spaces eq of
                                                  ('=':_) -> case fullValue other of
                                                              Just (val, leftover) -> Just ((attr, val), leftover)
                                                              _ -> Nothing
                                                  _ -> Nothing
            _ -> Nothing

manyAtt :: String -> Maybe (Attributes,String) 
manyAtt s = Just (attribs s)
attribs :: String -> (Attributes, String)
attribs str = 
  case attrib str of
    Just (ah, s) ->
      (ah:attr, rest)
      where (attr, rest) = attribs s
    Nothing -> ([], str)

-- Задача 5 -----------------------------------------
begTag :: String -> Maybe ((String,Attributes),String)
begTag [] = Nothing
begTag s 
  | (head s) /= '<' = Nothing
  | otherwise = case name (tail s) of
                Nothing -> Nothing
                Just (nm, rest) -> case manyAtt rest of
                                    Just (attr, ('>':leftover)) -> Just ((nm, attr), leftover)
                                    _ -> Nothing

endTag :: String -> Maybe (String,String) 
endTag s = if isEndTag s then
  case name (drop 2 s) of
    Just (nm, rest) -> Just (nm, (tail rest))
    _ -> Nothing
  else Nothing

isEndTag :: String -> Bool
isEndTag [_] = False
isEndTag [] = False
isEndTag (x:y:zs) = if x == '<' && y == '/' && '>' `elem` zs then True else False


-- Задача 6 -----------------------------------------
element :: String -> Maybe (XML,String) 
element s = getXML s
getXML :: String -> Maybe (XML,String)
getXML s =  if begTag s /= Nothing then
  Just ((Element (fst $ takeOne (begTag s)) (snd $ takeOne (begTag s)) []), "") 
  else Nothing
  where 
    takeOne Nothing = ("", [])
    takeOne (Just (x, _)) = x


xml :: String -> Maybe (XML,String)
xml s = 
  case text s of
    Just (x, rest) -> Just (Text x, rest)
    _ -> case element s of
          Just val -> Just val
          _ -> Nothing

manyXML :: String -> Maybe ([XML],String)
manyXML = undefined

-- Задача 7 -----------------------------------------
fullXML :: String -> Maybe XML 
fullXML s  = case element (spaces s) of  
 Just (xm,s1) -> if null (spaces s1) then Just xm else Nothing 
 Nothing      -> Nothing  

-- Тестові дані -------------------------------------------
-- Прості тести XML-об'єктів (без проміжків)
stst1, stst2, stst3 :: String
stst1 = "<a>A</a>"
stst2 = "<a x=\"1\"><b>A</b><b>B</b></a>"
stst3 = "<a>\
      \<b>\
        \<c att=\"att1\">text1</c>\
        \<c att=\"att2\">text2</c>\
      \</b>\
      \<b>\
        \<c att=\"att3\">text3</c>\
        \<d>text4</d>\
      \</b>\
    \</a>" 

-- Результати аналізу попередніх XML-об'єктів
x1, x2, x3 :: XML
x1 = Element "a" [] [Text "A"]
x2 = Element "a"
            [("x","1")]
            [Element "b" [] [Text "A"],
             Element "b" [] [Text "B"]]
x3 = Element "a" 
            [] 
            [Element "b" 
                     [] 
                     [Element "c"
                              [("att","att1")] 
                              [Text "text1"],
                      Element "c" 
                              [("att","att2")]
                              [Text "text2"]],
             Element "b" 
                     [] 
                     [Element "c" 
                              [("att","att3")] 
                              [Text "text3"],
                      Element "d" 
                              [] 
                              [Text "text4"]]]

casablanca :: String 
casablanca
  = "<film title=\"Casablanca\">\n  <director>Michael Curtiz</director>\n  <year>1942\
    \</year>\n</film>\n\n\n"

casablancaParsed :: XML 
casablancaParsed
  = Element "film" 
            [("title","Casablanca")] 
            [Text "\n  ",
             Element "director" [] [Text "Michael Curtiz"],
             Text "\n  ",
             Element "year" [] [Text "1942"],
             Text "\n"]



