module Parser where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L 
import qualified Text.Parsec.Token as T

import PTCL 

type Parser = Parsec Void String


-- TO DO : 1. How to parse data type with Nonterminal thing and terminal thing together


--
-- Lexer 
--

-- | spaceConsumer: consume the whitespace, newline,
--                  line comment out, block comment out 
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineCmnt blockCmnt 
  where lineCmnt = L.skipLineComment "#"
        blockCmnt = L.skipBlockComment "/*" "*/"

-- | Wrap parser for 'lexeme'
lexeme :: Parser a -> Parser a 
lexeme = L.lexeme spaceConsumer

-- | A helper to parse symbols (special string)
symbol :: String -> Parser String 
symbol = L.symbol spaceConsumer

-- | 'parens' parse things between parenthesis 
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | 'integer' parses an integer
integer :: Parser Int
integer = lexeme L.decimal

-- | 'comma' parses a comma ","
comma :: Parser String 
comma = symbol ","

-- | 'bar' parses a bar "|"
bar :: Parser String 
bar = symbol "|"

-- | newline parsers a newline "\n"
newline :: Parser String
newline = symbol "\n"

quote :: Parser Char
quote  = char '\"'


esc_quote :: Parser Char
esc_quote  = do {char '\"'; char '\"';}


-- | parses the reservedwords and identifiers 
reservedword :: String -> Parser ()
reservedword w = lexeme (string w *> notFollowedBy alphaNumChar)

-- | list of reserved words
reservedwords :: [String]
reservedwords = ["Atom","Int", "List", "data", "type", "decl", "is"]

-- | ? 
identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` reservedwords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

identifierUpper :: Parser String
identifierUpper = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> upperChar <*> many alphaNumChar
    check x = if x `elem` reservedwords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

identifierLower :: Parser String
identifierLower = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> lowerChar <*> many alphaNumChar
    check x = if x `elem` reservedwords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x
--
-- Parser for Build-in Type 
--

buildinType :: Parser Type
buildinType = TAtom <$ reservedword "Atom"
    <|> TInt <$ reservedword "Int"
    <|> TString <$ reservedword "String"
    <|> TList <$ reservedword "List"
    <|> TDef <$> identifier

--
-- Parser for User Defined Type
--

-- | parser for user defined type
definedType :: Parser DefinedType
definedType = typeType 
    <|> dataType

-- | parser for type constructor
typeType :: Parser DefinedType 
typeType =  do 
    reservedword "type"
    tName  <- typeName
    tp     <- buildinType
    return $ TypeT tName tp

-- | Parse the type name of user defined
typeName :: Parser TypeName 
typeName = identifierUpper


-- | Parser for data constructor 

dataType :: Parser DefinedType
dataType =  do 
    reservedword "data"
    tyname <- typeName
    void (symbol "=")
    dlist  <- dataCaseList
    return $ DataT tyname dlist 

-- | Parse a list of case in data constructor (separate by "|") 
-- dataCaseList :: Parser [(ConstructorName,[Type])]
-- dataCaseList = dataCaseWithArg `sepBy` bar
--     <|> dataCaseTerminal `sepBy` bar

dataCaseList :: Parser [(ConstructorName,[Type])]
dataCaseList = (dataCase) `sepBy` bar

-- dataCaseList :: Parser [(ConstructorName,[Type])]
-- dataCaseList = many1 

-- | parse the name of constructor
consName :: Parser TypeName 
consName = identifier

-- | parse each case of data type 
dataCase :: Parser (ConstructorName,[Type])
dataCase = do 
    cn <- consName
    option ((cn,[])) (do{
        ; void (symbol "(") 
        ; arglist <- typeList
        ; void (symbol ")")
        ; return (cn,arglist)})

-- | Parse a list of buildin Type seperate by comma
typeList :: Parser [Type]
typeList = buildinType  `sepBy` comma

--
-- * Parser for Type Declaration
--

-- | parse the names
varName :: Parser VarName
varName = identifierUpper

atomName :: Parser AtomName
atomName = identifierLower

predName :: Parser PredName
predName = identifierLower



stringName :: Parser String
stringName = do 
    char '"'
    str <- identifier 
    char '"'
    return str

-- stringLiteral

-- | parse the single declaration
typedecl :: Parser Dec
typedecl = do 
    reservedword "decl" 
    pn <- predName
    void (symbol "(") 
    tplist <- typeList  
    void (symbol ")")  
    return (pn, tplist)

--
-- * Parser for Prolog Program
--

-- (<$) :: Functor f => a -> f b -> f a
-- (<$>) :: Functor f => (a -> b) -> f a -> f b

rule :: Parser Rule 
rule = do 
   hd <- predicateT
   option (Head hd [])(do{
       ; void (symbol ":-")
       ; b <- body 
       ; void (symbol ".")
       ; return (Head hd b)})



-- dataCase :: Parser (ConstructorName,[Type])
-- dataCase = do 
--     cn <- consName
--     option ((cn,[])) (do{
--         ; void (symbol "(") 
--         ; arglist <- typeList
--         ; void (symbol ")")
--         ; return (cn,arglist)})

-- fact :: Parser Rule 
-- fact = 

-- headbody :: Parser Rule 
-- headbody = undefined 


definedTypeValue :: Parser DefinedTypeValue
definedTypeValue = undefined 

body :: Parser [BodyElem]
body = bodyElem `sepBy` comma

arg :: Parser Arg
arg =  Atom <$> atomName
   <|> LitI <$> integer
   <|> Var <$> varName
   <|> LitS <$> stringName
   -- <|> Def <$> typeName <$> definedTypeValue 
   -- <|> List 

list :: Parser Arg 
list = listNormal <|> listVar

listVar :: Parser Arg
listVar = do 
    void (symbol "[")
    vnhead <- varName
    void (symbol "|")
    vntail <- varName
    void (symbol "]")
    return $ List (Var vnhead :[Var vntail])

listNormal :: Parser Arg 
listNormal = do 
    void (symbol "[")
    arglist <- argList
    void (symbol "]")
    return $ List arglist   


predicateT :: Parser PredicateT
predicateT = do 
    pn <- predName 
    void (symbol "(")
    alist <- argList 
    void (symbol ")")
    return (Pred pn alist)

argList :: Parser [Arg]
argList = arg  `sepBy` comma

bodyElem :: Parser BodyElem 
bodyElem = isClause
   <|>Predicate <$> predicateT
   <|> Lit <$> integer
   <|> Ref <$> varName

   -- <|> Is ...
   -- <|> Oper 

isClause :: Parser BodyElem
isClause = do 
  left <- opExpr
  reservedword "is" 
  right <- opExpr
  return $ Is left right

opExpr :: Parser BodyElem
opExpr =  try compareExpr
      <|> (makeExprParser opTerm opt)


opt :: [[Operator Parser BodyElem]]
opt =
  [ [ InfixL (Oper Mult <$ symbol "*")
    , InfixL (Oper Div  <$ symbol "/") ]
  , [ InfixL (Oper Add  <$ symbol "+")
    , InfixL (Oper Sub  <$ symbol "-") ]
  ]

compareOp :: Parser Opt 
compareOp = (symbol "=" *> pure Eq)
  <|> (symbol "!=" *> pure Neq)
  <|> (symbol "<" *> pure Lt)
  <|> (symbol "<=" *> pure Leq) 
  <|> (symbol ">=" *> pure Gtq)
  <|> (symbol ">" *> pure Gt)

compareExpr :: Parser BodyElem
compareExpr = do 
  b1 <- bodyElem
  op <- compareOp
  b2 <- bodyElem
  return $ Oper op b1 b2

opTerm :: Parser BodyElem
opTerm = parens opExpr
  <|> Lit <$> integer 
  <|> Ref <$> varName





