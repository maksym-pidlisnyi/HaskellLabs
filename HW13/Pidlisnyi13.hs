{-# OPTIONS_GHC -Wall #-}
module Pidlisnyi13 where

import Text.ParserCombinators.Parsec

-- ������ 1 -----------------------------------------
fullBrace :: Parser()
fullBrace = do
  spaces
  brace 
  eof 
  return ()
bracesSq :: Parser ()
bracesSq = do
  symbol '['
  spaces
  brace
  symbol ']'
  spaces
  brace
bracesRou :: Parser ()
bracesRou = do
  symbol '('
  spaces
  brace
  symbol ')'
  spaces
  brace
bracesCur :: Parser ()
bracesCur = do
  symbol '{'
  spaces
  brace
  symbol '}'
  spaces
  brace
brace :: Parser ()
brace = bracesSq <|> bracesRou <|> bracesCur <|> spaces

balance  :: String -> Bool
balance str = either (const False) (const True) (parse fullBrace "" str)
   
-- ������ 2 ----------------------------------------- 
data Bexp = Bvalue Bool | Bvar Char | Not Bexp 
          | And Bexp Bexp | Or Bexp Bexp  deriving (Eq, Show)  

fullBe :: Parser Bexp 
fullBe = bexp <* eof 
anBexp :: String -> Maybe Bexp
anBexp str = case (parse fullBe "" str) of
                Left _   ->  Nothing
                Right ex -> Just ex
bexp :: Parser Bexp
bexp = chainl1 bcon (do
  {_ <- symbol '|'; return Or})
bcon :: Parser Bexp
bcon = chainl1 bdis (do
  {_ <- symbol '&'; return And})
bvar :: Parser Bexp
bvar = do{var <- letter; return (Bvar var)}
bdis :: Parser Bexp
bdis = 
  try (do 
        _ <- symbol '('
        x <- bexp; _ <- symbol ')'
        return x) <|> (do 
        { _ <- symbol '!'; y <- bdis; return (Not y)}) <|> (bvar) <|>
  (do 
   _ <- string "true"
   return (Bvalue True))
   <|> 
  (do 
    { _ <- string "false"; return (Bvalue False) })

-- ������ 3 ----------------------------------------- 
type Name       = String
type Attributes = [(String, String)]
data XML        =  Text String | Element Name Attributes [XML] deriving (Eq, Show)

fullXML :: Parser XML 
fullXML = do {
  spaces;
  el <- element;
  spaces;
  eof;
  return el
}
element :: Parser XML
element = do
    _ <- try (string "<")
    n <- name
    attr <- try (many attribute)
    _ <-  try (string ">")
    xml1 <- try (many xml)
    _ <- try (string "</")
    _ <- try (name)
    _ <- try (string ">")
    return (Element n attr xml1)
name :: Parser Name
name = do
  lt <- letter
  ns <- many (letter <|> digit <|> oneOf ".-")
  return $ lt:ns
xml :: Parser XML
xml = try (element <|> text)
text :: Parser XML
text = do {
  t <- many (noneOf "<>");
  return (Text t)
}
attribute :: Parser (String, String)
attribute = do {
  spaces;
  n <- name;
  spaces;
  _ <- symbol '=';
  spaces;
  val <- fullVal;
  return (n, val)
}
fullVal :: Parser String
fullVal = do
  _ <- string "\""
  val <- value 
  symbol '"'
  return val
value :: Parser String
value = many (noneOf "\"")

anXML :: String -> Maybe XML
anXML str = case (parse fullXML "" str) of
               Left _ -> Nothing
               Right res -> Just res

----------------  ���� SPL  ------------   
data Value = I Int  | B Bool deriving (Show, Eq)
data Exp = Var String      -- �����
         | Const Value     -- ���������
         | Op Exp Bop Exp  -- ��������
                 deriving (Show, Eq)

-- ������� (2-���������) ���������
data Bop =  Plus | Minus | Times | Div   
          | Gt | Ge | Lt | Le| Eql | Ba | Bo
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

{- ������� 
  symbol = ';' | '{' | '}' | '(' | ')' 
  identif=  char {digit | char}
  keyword= "int" | "bool" | "read" | "write" |"if" | "while" | "for" | "true" | "false"
  iden   =  identif .... not "int" "bool" "read" ""write" "if" "while" "for" "true" "false"
  number = digit { digit }.
  mulOp  = "*" | "/".
  addOp  = "+" | "-".
  relOp  = "<" | "<=" | ">" | ">=" | "==" 
  disOp  = "&" 
  conOp  = "|"
  typev  = "int" | "bool" 
-}
iden :: Parser String
iden = try( do {nm <- identif;
                if (any(nm==) ["int","bool","read","write","if","while","for","true","false", "do"])
                    then unexpected ("reserved word " ++ show nm)
                    else return nm 
               } ) 

oper  :: String -> Bop -> Parser Bop
oper str bop = do {_ <- string str; return bop}

mulOp :: Parser Bop   
mulOp = (oper "*" Times) <|> (oper "/" Div)

disOp :: Parser Bop   
disOp = (oper "&" Ba)

conOp :: Parser Bop   
conOp = (oper "|" Bo)

-- ��������� �� "�������" ������� � ����		
lexem :: Parser a -> Parser a
lexem p = do {a <- p; spaces; return a}

--   :type Op -----> Exp -> Bop -> Exp -> Exp 
--   :type flip Op -------> Bop -> Exp -> Exp -> Exp         
expOp :: Parser Bop -> Parser (Exp -> Exp -> Exp)
expOp p = do {x <- lexem p; return (flip Op x)}

symbol :: Char ->  Parser ()
symbol ch = lexem (char ch >> return ())

keyword :: String -> Parser ()
keyword st = try( lexem( string st >> notFollowedBy alphaNum)) 

typev :: Parser Type 
typev = do {keyword "int"; return It}
        <|> do {keyword "bool"; return Bt} 

-- ������ 4 -----------------------------------------
identif :: Parser String
identif = do
  l <- letter
  ls <- many (letter <|> digit)
  return (l:ls)

number :: Parser Int
number = do
  n <- many1 digit
  return (read n)
 
addOp :: Parser Bop  
addOp = (oper "+" Plus) <|> (oper "-" Minus)

relOp :: Parser Bop  
relOp = (oper "==" Eql) <|> (oper ">" Gt)<|> (oper ">=" Ge) <|> (oper "<" Lt) <|> (oper "<=" Le) 

{- ������ 
  factor = '(' expr ')' | number | "true" | "false" | iden
  term   = factor { mulOp factor }
  relat  = term { addOp term }
  conj   = relat [relOp relat] 
  disj   = conj { conOp conj}   
  expr   = disj { disOp disj}
-}
factor :: Parser Exp
factor = do { symbol '('; x <- expr; symbol ')'; return x}
     <|> do {nm <- lexem number; return (Const (I nm))}
     <|> do {keyword "true"; return (Const (B True))}
     <|> do {keyword "false"; return (Const (B False))}
     <|> do {cs <- lexem iden; return (Var cs) }
     <?> "factor"

-- ������ 5 -----------------------------------------
term :: Parser Exp     
term = chainl1 factor (expOp mulOp)  

relat :: Parser Exp
relat = chainl1 term (expOp addOp)

conj :: Parser Exp
conj = chainl1 relat (expOp relOp)

disj :: Parser Exp
disj = chainl1 conj (expOp conOp)

expr :: Parser Exp
expr = chainl1 disj (expOp disOp)

{- ���������
  stmt   = "for" forSt | "while" whileSt | "if" ifSt 
         | "read" inSt | "write" outSt | iden assSt | blockSt  
  forSt  = '(' stmt ';' expr ';' stmt ')' stmt 
  whileSt= '(' expr ')' stmt 
  ifSt   = '(' expr ')' stmt 
  inSt   = iden 
  outSt  = expr
  assSt  = "++" | ":=" expr 
  blockSt= '{' {defin} listSt '}' 
  defin  = type listId ';'
  listId = iden {',' iden}
  listSt = stmt {';' stmt}  
  program= stmt eos 
-}   
stmt :: Parser Stmt 
stmt = do {keyword "for"; forSt}
       <|> do {keyword "while"; whileSt}
       <|> do {keyword "if"; ifSt}
       <|> do {keyword "read"; inSt}
       <|> do {keyword "write"; outSt}
       <|> do {var <- lexem iden; assignSt var}
       <|> blockSt
       <?> "statement"

-- ������ 6 -----------------------------------------
forSt :: Parser Stmt  
forSt = do
  symbol '('
  st <- lexem stmt
  symbol ';'
  se <- lexem expr
  symbol ';'
  sn <- lexem stmt
  symbol ')'
  ss <- lexem stmt
  return (For st se sn ss)

whileSt :: Parser Stmt               
whileSt = do
  symbol '('
  se <- lexem expr
  symbol ')'
  ss <-  lexem stmt
  return (While se ss)
              
ifSt :: Parser Stmt              
ifSt = do
  symbol '('
  se <- lexem expr
  symbol ')'
  ss <- lexem stmt
  return (If se ss)

inSt :: Parser Stmt              
inSt = do
  ss <- lexem iden
  return (Read ss)  

outSt :: Parser Stmt              
outSt = do
  se <- lexem expr
  return (Write se)

assignSt :: String -> Parser Stmt 
assignSt val = do {
  symbol ':';
  symbol '=';
  se <- lexem expr;
  return (Assign val se)
} <|> do {
  symbol '+';
  symbol '+';
  return (Incr val)
 }
                            
blockSt :: Parser Stmt
blockSt = do
  symbol '{'
  def <- many defin
  list <- listSt
  symbol '}'
  return (Block def list)
defin :: Parser (String, Type)
defin = do
  typ <- lexem typev
  ide <- lexem iden
  symbol ';'
  return (ide, typ)
listSt :: Parser [Stmt]
listSt = do
  ss <- lexem stmt
  sxs <- many (do
    symbol ';'
    lexem stmt)
  return (ss : sxs)
               
---------------------------------------------	
-- ������� �������
---------------------------------------------				
program :: Parser Stmt 
program = do {spaces; r <- stmt; eof; return r}

parseSPL :: String -> Either ParseError Program
parseSPL s = parse program "" s

---------------------------------------------
--- ���� ��� ����������
--------------------------------------------- 
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

squareRoot :: String
squareRoot =
   "{int a, b; \
   \ read a; b := 0; \
   \ if (a>= 0)\
   \    {bool c; c:=true; while(c) {b++; c:= a >= b*b}\
   \    };\
   \  write (b-1)\
   \ }"

power :: String
power =
   "{ int b; int e; int out; b := 6; e := 5; out:= 1;\
   \  for (i:=0; i<e; i++) out := out*b   \
   \}"

squareRootAST :: Program
squareRootAST = Block [("a",It),("b",It)]
                   [ Read "a", Assign "b" (Const (I 0))
                   , If (Op (Var "a") Ge (Const(I 0))) 
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

fibonacci :: String
fibonacci = 
 " {int in, out; read in; out := 0; \n\
   \if (in>=0){int f0, f1,c; \n\
   \           f0 := 1; f1 := 1; out := 1; \n\
   \           if(in>1) \n \
   \              for (c := 1; c < in; c++) {\n\
   \                   out := f0 + f1; f0 := f1; f1 := out\n\
   \              }\n\
   \          }; \n\
   \write out \n\
  \}"
  
fibonacciAST :: Program
fibonacciAST = 
    Block [("in",It), ("out",It)]
          [ Read "in",  Assign "out" (Const (I 0))
          , If (Op (Var "in") Ge (Const(I 0))) 
               (Block [("f0",It), ("f1",It), ("c",It)]
                           [Assign "f0" (Const (I 1)), Assign "f1" (Const (I 1)),
                            Assign "out" (Const (I 1)),
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
                            ]
                )
          , Write (Var "out")
          ]

