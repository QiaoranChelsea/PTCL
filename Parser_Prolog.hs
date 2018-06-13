module Parser_Prolog where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L 
import qualified Text.Parsec.Token as T

import Text.Megaparsec.Pos

import Types 
import Parser_Lexer
import Parser_Types

--
-- * Parser for Prolog Program
--

rule  :: Parser (Rule,Line)
rule = do 
    rl  <- rule'
    pos <- getPosition
    return (rl,unPos (sourceLine pos))

-- | parse a single rule 
rule' :: Parser Rule 
rule' = do 
   hd <- predFunA
   option (Head hd [])(do{
       ; void (symbol ":-")
       ; b <- andExpr 
       ; return (Head hd [b])})


-- | parse a list of bodyelem
-- bodyElemList :: Parser [BodyElem]
-- bodyElemList  = bodyElem `sepBy` comma


-- | parse the argument Name 
argument :: Parser Argument
argument = try optAExpr
   <|> try (Func <$> predFunA)
   <|> try (Atom <$> atomName)
   <|> try (LitI <$> integer)
   <|> try (LitS <$> stringName)
   <|> try listNormal
   <|> try listVar
   <|> try (Var <$> varName)
   <|> parens argument

predFunA :: Parser PredFunA 
predFunA = do 
    fn <- funcName 
    void (symbol "(")
    arglist <- argumentList 
    void (symbol ")") 
    return (fn,arglist)

argumentList :: Parser [Argument]
argumentList = argument  `sepBy` comma

-- | parse a list 
list :: Parser Argument 
list = listNormal <|> listVar

-- | parse a list argument  used as a variable 
listVar :: Parser Argument
listVar = do 
    void (symbol "[")
    vnhead <- varName
    void (symbol "|")
    vntail <- varName
    void (symbol "]")
    return $ List (Var vnhead :[Var vntail])

-- | Parse a list [] 
listNormal :: Parser Argument 
listNormal = do 
    void (symbol "[")
    arglist <- argumentList
    void (symbol "]")
    return $ List arglist   

-- | parse mathmatic clause with its operation
optAExpr :: Parser Argument
optAExpr = makeExprParser optATerm optA

optA :: [[Operator Parser Argument ]]
optA =
  [ [ InfixL (OperA Mult <$ symbol "*")
    , InfixL (OperA Div  <$ symbol "/") ]
  , [ InfixL (OperA Add  <$ symbol "+")
    , InfixL (OperA Sub  <$ symbol "-") ]
  ]

optATerm :: Parser Argument
optATerm = parens optAExpr
  <|> Var <$> varName 
  <|> LitI <$> integer
  <|> LitS <$> stringName

-- -- | parse a predicate / single fact
-- predicateT :: Parser PredFunA
-- predicateT = do 
--     pn <- predName 
--     void (symbol "(")
--     blist <- bodyElemList 
--     void (symbol ")")
--     return (Pred (pn blist))

bodyElem :: Parser BodyElem 
bodyElem =   try isClause
   <|> try (Pred <$> predFunA)
   <|> try compareExpr

-- | Parse isClause such as Y is 4*3
isClause :: Parser BodyElem
isClause = do 
  left <- argument  
  reservedword "is" 
  right <- argument
  return $ Is left right

-- | Parse the compare term
compareOp :: Parser OptC 
compareOp = (symbol "=" *> pure Eq)
  <|> (symbol "!=" *> pure Neq)
  <|> (symbol "<" *> pure Lt)
  <|> (symbol "<=" *> pure Leq) 
  <|> (symbol ">=" *> pure Gtq)
  <|> (symbol ">" *> pure Gt)

compareExpr :: Parser BodyElem
compareExpr = do 
  b1 <- optATerm
  op <- compareOp
  b2 <- optATerm
  return $ OperC op b1 b2

andExpr :: Parser BodyElem
andExpr = makeExprParser bodyElem andOperators

-- | define the operator(,) for AttrList, in the case of concatenate the list
andOperators :: [[Operator Parser BodyElem]]
andOperators =
  [[ InfixL (And <$ symbol ",")]]





