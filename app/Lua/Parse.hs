{-# LANGUAGE FlexibleContexts #-}
module Lua.Parse where

import Lua.Core

import Data.Functor.Identity
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Text.Parsec.Prim hiding (State, try)
import Text.Parsec.Token hiding (parens)
import Text.Parsec.Expr 
import Control.Monad


{-
second parameter which in the real world would be the name of the file whose contents you are parsing, and is just used in error messages by Parsec to give you that piece of extra information
-}
type Parser = ParsecT String () Identity

-- Lexicals 

keyword :: String -> Parser String
keyword s = do string s
               spaces
               return s

parens :: Parser a -> Parser a
parens p = do keyword "("
              pp <- p
              keyword ")"
              return pp

int :: Parser Int
int = do digits <- many1 digit <?> "an integer"
         spaces
         return (read digits :: Int)

var :: Parser String
var = do v <- many1 letter <?> "an identifier"
         spaces
         return v



{-
data Exp = 
     | UnopExp String Exp    
     | BinopExp String Exp Exp  
     | TableConstructor [(Exp,Exp)]  -- evaluate to a TableVal     
     | TableLookUpExp Table Exp -- todo: lookup key in table 
  deriving (Show, Eq)
-}

-- Expressions
nilExp :: Parser Exp 
nilExp =  keyword "nil" >> return (NilExp)

intExp :: Parser Exp
intExp = do i <- int
            return $ IntExp i

boolExp :: Parser Exp
boolExp =    ( keyword "true"  >> return (BoolExp True)  )
         <|> ( keyword "false" >> return (BoolExp False) )

varExp :: Parser Exp
varExp = do v <- var
            return $ VarExp v

strExp :: Parser Exp 
strExp = do char '"'
            s <- many $ noneOf ['"']
            char '"'
            spaces
            return $ StrExp s



tableConstructor :: Parser Exp
tableConstructor = undefined


expr :: Parser Exp
expr = buildExpressionParser table atom <?> "expression"



atom :: Parser Exp
atom = nilExp 
   <|> intExp
   <|> boolExp  
   <|> varExp
   <|> strExp 
   <|> parens expr


{-
Precedence and associativity 
or
and
<     >     <=    >=    ~=    ==
|
~
&
<<    >>
..
+     -
*     /     //    %
unary operators (not   #     -     ~)
^
-}

-- fun should be Exp -> Exp -> Exp 
-- Todo: add function signature 
binary  name fun assoc = Infix (do{ keyword name; return fun }) assoc
prefix  name fun       = Prefix (do{ keyword name; return fun })
table = [ [binary "^" (BinopExp "^") AssocRight]
        , [prefix op (UnopExp op) | op <- ["not", "#", "-"]]
        , [binary op (BinopExp op) AssocLeft | op <- ["*","/","//","%"]]
        , [binary op (BinopExp op) AssocLeft | op <- ["+","-"]]
        , [binary ".." (BinopExp "..") AssocRight]
        , [binary op (BinopExp op) AssocLeft | op <- ["<",">","<=",">=","~=","=="]]
        , [binary "and" (BinopExp "and") AssocLeft]
        , [binary "or" (BinopExp "or") AssocLeft]]

{-
--- ### Statements
data Stmt = AssignStmt [String] [Exp] -- variable assignment, support multiple assignment
          | IfStmt [(Exp, Stmt)] Stmt -- conditional statements
          | BlockStmt [Stmt] -- sequence statements, act like semi-colon 
          | WhileStmt Exp Stmt -- while exp do block end 
          | RepeatStmt Stmt Exp -- repeat block until exp， condition exp can refer to local variables declared inside the loop block 
          | ForNumStmt String Exp Exp Exp -- numerical for loop 
          | PrintStmt Exp -- printing
          | AssignLocalStmt [String] [Exp] -- local varable declaration. scope within the block 
    deriving (Show, Eq)
-}
printStmt :: Parser Stmt
printStmt = do try $ keyword "print"
               e <- parens expr 
               return $ PrintStmt e

quitStmt :: Parser Stmt
quitStmt = do try $ keyword "quit"
              return QuitStmt

stmt :: Parser Stmt
stmt = quitStmt <|> printStmt
{-
stmt = 
   <|> printStmt
   <|> ifStmt
   <|> blockStmt
   <|> try setStmt
   -}