{-# OPTIONS_GHC -Wall #-}
module Pidlisnyi12 where

import Data.Maybe
import Data.Char(isDigit)

data Value = I Int  | B Bool deriving (Show, Eq)
data Exp = Var String      -- �����
         | Const Value     -- ���������
         | Op Exp Bop Exp  -- ��������
                 deriving (Show, Eq)
-- ������� (2-���������) ���������
data Bop =  Plus | Minus | Times | Div   
          | Gt | Ge | Lt | Le| Eql | And | Or
            deriving (Show, Eq)

data Stmt = Assign String Exp
          | Read String 
          | Write Exp
          | Incr String
          | If Exp Stmt 
          | While Exp Stmt       
          | For Stmt Exp Stmt Stmt
          | Block [(String,Type)] [Stmt]        
          deriving (Show, Eq)
data Type = It | Bt deriving (Show, Eq)
type Program = Stmt

type StateW = ([String], [(String,Value)], [String])

type VarEnv  = [(String,Type)]

-- ������ 1 -----------------------------------------
getValue::  StateW -> String -> Value
getValue st iD = fromJust (lookup iD (snd3 st))
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x
thr3 :: (a, b, c) -> c
thr3 (_, _, x) = x

updValue :: StateW -> String -> Value -> StateW
updValue st iD v = ((fst3 st), map (\x -> if fst x == iD then (iD, v) else x) (snd3 st), (thr3 st))

-- ������ 2 ----------------------------------------- 
readValue :: StateW -> Type -> (StateW,Value)
readValue st (It)
  | null (fst3 st) = (st, (I 0))
  | isNumber (head (fst3 st)) = ((tail $ (fst3 st), (snd3 st), (thr3 st)), (I (read (head (fst3 st)))))
  | otherwise = (st, (I 0))
readValue st (Bt)
  | null (fst3 st) = (st, (B False))
  | (head (fst3 st)) == "True" || (head (fst3 st)) == "False" = ((tail $ (fst3 st), (snd3 st), (thr3 st)), (stringToVal (head (fst3 st))))
  | otherwise = (st, (B False))
isNumber :: String -> Bool
isNumber ""  = False
isNumber "." = False
isNumber xs  =
  case dropWhile isDigit xs of
    ""       -> True
    ('.':ys) -> all isDigit ys
    _        -> False
stringToVal :: String -> Value
stringToVal s 
  | s == "True" = (B True)
  |otherwise = (B False)
stringToType :: String -> Type
stringToType s 
  | s == "True" && s == "False" = Bt
  | otherwise = It

-- ������ 3 -----------------------------------------
writeValue :: StateW -> Value -> StateW 
writeValue st v = ((fst3 st), (snd3 st), ((thr3 st) ++ [valToString v]))
valToString :: Value -> String
valToString v = filter (not . (`elem` "IB ")) (show v)
  
-- ������ 4 ----------------------------------------- 
evExp :: StateW -> Exp -> Value
evExp st (Op exp1 Plus exp2) = opArithmetic (evExp st exp1) (evExp st exp2) (+)
evExp st (Op exp1 Minus exp2) = opArithmetic (evExp st exp1) (evExp st exp2) (-)
evExp st (Op exp1 Times exp2) = opArithmetic (evExp st exp1) (evExp st exp2) (*)
evExp st (Op exp1 Div exp2) = opArithmetic (evExp st exp1) (evExp st exp2) (div)
evExp st (Op exp1 And exp2) = opBool (evExp st exp1) (evExp st exp2) (&&)
evExp st (Op exp1 Or exp2) = opBool (evExp st exp1) (evExp st exp2) (||)
evExp st (Op exp1 Eql exp2) = opCompare (evExp st exp1) (evExp st exp2) (==)
evExp st (Op exp1 Lt exp2) = opCompare (evExp st exp1) (evExp st exp2) (<)
evExp st (Op exp1 Gt exp2) = opCompare (evExp st exp1) (evExp st exp2) (>)
evExp st (Op exp1 Le exp2) = opCompare (evExp st exp1) (evExp st exp2) (<=)
evExp st (Op exp1 Ge exp2) = opCompare (evExp st exp1) (evExp st exp2) (>=) 
evExp st (Var x) = getValue st x
evExp _ (Const x) = x
opArithmetic :: Value -> Value -> (Int -> Int -> Int) -> Value
opArithmetic (I int1) (I int2) opr = I (opr int1 int2)
opArithmetic _ _ _ = error "Type mismatch!"
opBool :: Value -> Value -> (Bool -> Bool -> Bool) -> Value
opBool (B i1) (B i2) opr = B (opr i1 i2)
opBool _ _ _ = error "Type mismatch!"
opCompare :: Value -> Value -> (Int -> Int -> Bool) -> Value
opCompare (I i1) (I i2) opr = B (opr i1 i2)
opCompare _ _ _ = error "Type mismatch!"

-- ������ 5 -----------------------------------------
evStmt :: StateW -> Stmt -> StateW 
evStmt st (Assign s e) = updValue st s (evExp st e)
evStmt st (Write e) = writeValue st (evExp st e)
evStmt st (Read s) = fst $ readValue (updValue st s (snd $ readValue st (stringToType s))) (stringToType s)
evStmt st (Incr s) = updValue st s $ I $ val + 1 where (I val) = getValue st s
evStmt st (If e stm) = if bol then evStmt st stm else st
    where (B bol) = evExp st e
evStmt st (While e stm)
  | (evExp st e == B True) = evStmt st (While e stm)
  | otherwise = st
evStmt st (For stm1 e stm2 stm3)
  | evExp (evStmt st stm1) e == B True = evStmt newSt (For asStm e stm2 stm3)
  | otherwise = st where
    asStm = (Assign "" (Var ""))
    newSt = evStmt (evStmt (evStmt st stm1) stm2) stm3
evStmt st (Block blocks stmts) = foldl (evStmt) first stmts
    where first = ((fst3 st),[(i, val) | (i, typ) <- blocks, 
                  let val = if typ == Bt then (B False) else (I 0)] ++ (snd3 st), (thr3 st))

-- ������ 6 -----------------------------------------
evProgram :: Program -> [String] -> [String]
evProgram prog s = thr3 (evStmt (s,[],[]) prog)
evProgram1 :: Program -> [String] -> StateW
evProgram1 prog s = (evStmt (s,[],[]) prog)

-- ������ 7 -----------------------------------------
iswfOp :: Bop -> [Type] -> Maybe Type 
iswfOp bop xs
  | (bop == Plus || bop == Minus || bop == Times || bop == Div) &&
      length xs == 2 && ((head xs) == It && (last xs) == It) = Just It
  | (bop == Eql || bop == And || bop == Or) &&
      length xs == 2 &&
      (((head xs) == It && (last xs) == It) ||
       ((head xs) == Bt && (last xs) == Bt)) = Just Bt
  | length xs == 2 && ((head xs) == It && (last xs) == It) = Just Bt
  | otherwise = Nothing

-- ������ 8 -----------------------------------------
iswfExp :: Exp -> VarEnv -> Maybe Type 
iswfExp (Const (B _)) _ = Just Bt
iswfExp (Const (I _)) _ = Just It
iswfExp (Var x) var = lookup x var 
iswfExp (Op exp1 bop exp2) var
  | (iswfExp exp1 var) == Nothing || (iswfExp exp2 var) == Nothing = Nothing
  | otherwise = iswfOp bop [fromJust (iswfExp exp1 var), fromJust (iswfExp exp2 var)]

-- ������ 9 -----------------------------------------
iswfStmt :: Stmt -> VarEnv -> Bool 
iswfStmt _ [] = True
iswfStmt (Assign _ _) _ = True
iswfStmt (Write _) _ = True
iswfStmt (Read _) _ = True
iswfStmt (Incr s) v = if ((varsToType v s) == It) then True else False
iswfStmt (If e smt) v = (iswfStmt smt v) && (iswfExp e v) == Just Bt 
iswfStmt (While e smt) v = if ((iswfStmt smt v) && ((iswfExp e v) == Just Bt)) then True else False
iswfStmt (For smt1 e smt2 smt3) v = (iswfStmt smt1 v) &&
 (iswfStmt smt2 v) && (iswfStmt smt3 v) && (iswfExp e v) == Just Bt 
iswfStmt (Block vars stmts) v = and (map ((flip iswfStmt) (vars ++ v)) stmts)
varsToType :: VarEnv -> String -> Type
varsToType v s = snd (head (filter (\x -> fst x == s) v))

--------------------------------
iswfProgram :: Program -> Bool 
iswfProgram st = iswfStmt st []

-- �������� -------------------------------------------

{- ������� ��� ��� �������� b � e, 
   ���� ���� �������, �� � ������ out
   ���������� �������� b � ������� e. 
   �������� out ����������

   { int b, e, out;  
     read b; read e; out:= 1;
	 if (b>0 & e>0)
	   {int i; for (i:=0; i<e; i++) out := out*b}; 
     write out  
   }
-}
power :: Program
power = Block [("b",It),("e",It),("out",It)]
              [ Read "b", Read "e", Assign "out" (Const(I 1))
              , If (Op (Op (Var "b") Gt (Const(I 0)))
                       And
                       (Op (Var "e") Gt (Const(I 0))) )
                   (Block [("i",It)]
                        [For (Assign "i" (Const(I 0))) (Op (Var "i") Lt (Var "e")) (Incr "i")
                              (Assign "out" (Op (Var "out") Times (Var "b")))
                        ]
                   )
              , Write (Var "out") 
              ]

{- ������� ���� �������� �,
   ���� ���� ����"����, �� ��������� ������ �������� 
   ����� ����������� � �������� a � �������� ����.
   ��� ��"������ � ���������� -1.   
   
   { int a, b; 
     read a; b := 0;
	 if (a>=0)
	   {bool c; c:=true; 
	    while(c) {b++; c:= a >= b*b}
	   };
     write (b-1)
   } 	
-} 
squareRoot :: Program
squareRoot = Block [("a",It),("b",It)]
                   [ Read "a", Assign "b" (Const (I 0))
                   , If (Op (Var "b") Ge (Const(I 0)))
                        (Block [("c", Bt)] 
                               [Assign "c" (Const (B True)),
                                While (Var "c")
                                 (Block []
                                   [(Incr "b"), 
                                    Assign "c" (Op (Var "a") Ge (Op (Var "b") Times (Var "b")))
                                   ])
                               ]
                        )
                   , Write (Op (Var "b") Minus (Const (I 1)))
                   ]

{- ������� ���� �������� ,
   ���� ���� ����"����, �� �������� �������� ����� 
   Գ������� � ������ out � �������� ����.
   ��� ��"������ � ���������� 0.
   
   {int in, out; 
    read in; out := 0; 
	if (in>=0) 
      {int f0, f1, c; 
	   f0 := 1; f1 := 1; out := 1;
       if (in > 1)  
         for (c := 1; c < in; c++) 
		   {out := f0 + f1; f0 := f1; f1 := out}
	 };
    write out	 
  }
-}
fibonacci :: Program
fibonacci = 
    Block [("in",It), ("out",It)]
          [ Read "in",  Assign "out" (Const (I 0))
          , If (Op (Var "in") Gt (Const(I 1))) 
               (Block [("f0",It), ("f1",It), ("c",It)]
                     [Assign "f0" (Const (I 1)), Assign "f1" (Const (I 1)),
                      If (Op (Var "in") Gt (Const (I 1)))
                         (For (Assign "c" (Const (I 1)))
                             (Op (Var "c") Lt (Var "in")) 
                             (Incr "c")
                             (Block []
                                    [Assign "out" (Op (Var "f0") Plus (Var "f1"))
                                    , Assign "f0" (Var "f1")
                                    , Assign "f1" (Var "out")
                                    ]
                              )
                         )
                     ])
          , Write (Var "out")
          ]



