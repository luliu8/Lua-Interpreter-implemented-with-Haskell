{-# LANGUAGE FlexibleContexts #-}
module Lua.Parse where

import Lua.Core

import Data.Functor.Identity
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Text.Parsec.Prim hiding (State, try)
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
data Exp = NilExp
     | IntExp Int
     | FloatExp Float
     | BoolExp Bool
     | StrExp String -- literal string, simply evaluate to StrVal 
     | Unop String Exp    
     | Binop String Exp Exp 
     | VarExp String  -- lookup variable name in environment 
              -- before first assignment to a variable, value is nil 
     | TableConstructor [(Exp,Exp)]  -- evaluate to a TableVal     
     | TableLookUpExp Table Exp -- todo: lookup key in table 
     | ParensExp Exp -- ( exp )
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
            return $ StrExp s

expr :: Parser Exp
-- todo, add other types of expressions 
expr = atom

atom :: Parser Exp
atom = nilExp 
   <|> intExp
   <|> boolExp  -- why need to add try 
   <|> varExp
   <|> strExp 
   <|> parens expr{-
--- ### Statements
data Stmt = AssignStmt [String] [Exp] -- variable assignment, support multiple assignment
          | AssignLocalStmt [String] [Exp] -- local varable declaration. scope within the block 
          | IfStmt [(Exp, Stmt)] Stmt -- conditional statements
          | BlockStmt [Stmt] -- sequence statements, act like semi-colon 
          | WhileStmt Exp Stmt -- while exp do block end 
          | RepeatStmt Stmt Exp -- repeat block until exp， condition exp can refer to local variables declared inside the loop block 
          | ForNumStmt String Exp Exp Exp -- numerical for loop 
          | PrintStmt Exp -- printing
          | QuitStmt -- exit the interpreter 
    deriving (Show, Eq)
-}
printStmt :: Parser Stmt
printStmt = do try $ keyword "print"
               e <- parens expr 
               return $ PrintStmt e


stmt :: Parser Stmt
stmt = printStmt
{-
stmt = quitStmt
   <|> printStmt
   <|> ifStmt
   <|> blockStmt
   <|> try setStmt
   -}