module Lua.Parse where

import Lua.Core

import Data.Functor.Identity
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Text.Parsec.Prim hiding (State, try)
import Text.Parsec.Token hiding (parens, symbol)
import Text.Parsec.Expr 
import Control.Monad


{-
second parameter which in the real world would be the name of the file whose contents you are parsing, and is just used in error messages by Parsec to give you that piece of extra information
-}
type Parser = ParsecT String () Identity

-- Lexicals 

symbol :: String -> Parser String
symbol s = do string s
              spaces
              return s

parens :: Parser a -> Parser a
parens p = do symbol "("
              pp <- p
              symbol ")"
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
nilExp =  symbol "nil" >> return (NilExp)

intExp :: Parser Exp
intExp = do i <- int
            return $ IntExp i

boolExp :: Parser Exp
boolExp =    ( symbol "true"  >> return (BoolExp True)  )
         <|> ( symbol "false" >> return (BoolExp False) )

varExp :: Parser Exp
varExp = do v <- var
            return $ VarExp v

strExp :: Parser Exp 
strExp = do char '"'
            s <- many $ noneOf ['"']
            char '"'
            spaces
            return $ StrExp s


fieldSep :: Parser String 
fieldSep = (symbol ",") <|> (symbol ";")

--currently only support the most general form of table constructor  {[Exp] = Exp }
tableConstructor :: Parser Exp   
tableConstructor = do try $ symbol "{"
                      fieldList <- (do symbol "["
                                       keyExp <- expr
                                       symbol "]"
                                       symbol "="
                                       valExp <- expr
                                       return (keyExp, valExp)
                                    )
                                    `sepBy` fieldSep
                      symbol "}"
                      return $ TableConstructor fieldList

tableLookUpExp :: Parser Exp 
tableLookUpExp = do t <- var 
                    symbol "["
                    keyExp <- expr 
                    symbol "]"
                    return $ TableLookUpExp (VarExp t) keyExp



expr :: Parser Exp
expr = buildExpressionParser table atom <?> "expression"



atom :: Parser Exp
atom = nilExp 
   <|> intExp
   <|> try boolExp  
   <|> try tableLookUpExp
   <|> try strExp 
   <|> parens expr
   <|> tableConstructor 
   <|> try varExp


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
binary  name fun assoc = Infix (do{ symbol name; return fun }) assoc
prefix  name fun       = Prefix (do{ symbol name; return fun })
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
printStmt = do try $ symbol "print"
               e <- parens expr 
               return $ PrintStmt e

quitStmt :: Parser Stmt
quitStmt = do try $ symbol "quit"
              return QuitStmt

assignStmt :: Parser Stmt 
assignStmt = do keyExpList <- varExp `sepBy` (symbol ",")
                symbol "="
                valExpList <- expr `sepBy` (symbol ",")
                return $ AssignStmt keyExpList valExpList
tableAssignStmt ::Parser Stmt 
tableAssignStmt = do e1 <- varExp 
                     symbol "["
                     e2 <- expr 
                     symbol "]"
                     symbol "="
                     e3 <- expr 
                     return $ TableAssignStmt e1 e2 e3 



stmt :: Parser Stmt
stmt =  quitStmt 
    <|> printStmt
    <|> try tableAssignStmt 
    <|> try assignStmt 

{-
stmt = 
   <|> printStmt
   <|> ifStmt
   <|> blockStmt
   <|> try setStmt
   -}