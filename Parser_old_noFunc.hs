module Parser where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L 
import qualified Text.Parsec.Token as T



import Types 

type Parser = Parsec Void String

-- TO DO : 1. and in body 
--         2. underscore


-- definedType
-- typedecl
-- rule

-- parseFromFile definedType "sample.pl"
-- parseFromFile p file = runParser p file <$> readFile file

-- parseFromFile p file = runParser p file <$> readFile file

-- main = parseFomFile definedTypes "sample.pl"
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

period :: Parser String 
period = symbol "."

underscore :: Parser String 
underscore = symbol "_"

-- | newline parsers a newline "\n"
-- newline' :: Parser Char
-- newline' = char '\n'

quote :: Parser Char
quote  = char '\"'


esc_quote :: Parser Char
esc_quote  = do {char '\"'; char '\"';}

--  (*>) :: f a -> f b -> f b
-- | parses the reservedwords and identifiers 
reservedword :: String -> Parser ()
reservedword w = lexeme (string w *> notFollowedBy alphaNumChar)

-- | list of reserved words in scope of type
rwTypes :: [String]
reservedwords = ["atom","int", "list", "stirng"]

-- | list of reserved words in global
rwGlobal :: [String ]
rwGlobal = ["data", "type", "decl", "is"]

-- | 
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
buildinType = TAtom <$ reservedword "atom"
    <|> TInt <$ reservedword "int"
    <|> TString <$ reservedword "string"
    <|> TList <$ reservedword "list"
    <|> TDef <$> identifier


-- typeVar :: Parser TypeVar 
-- typeVar = do 
   
--
-- Parser for User Defined Type
--

-- definedTypes :: Parser [DefinedType]
-- definedTypes = definedType `sepBy` newline'

-- | parser for user defined type
definedType' :: Parser DefinedType
definedType' = typeType 
    <|> dataType

-- | parser for type constructor
typeType1 :: Parser DefinedType 
typeType1 =  do 
    reservedword "type"
    tName  <- typeName
    void (symbol "=")
    tp     <- buildinType
    return $ TypeT tName tp

typeType2 :: Parser DefinedType 
typeType2 =  do 
    reservedword "type"
    tName  <- typeName
    void (symbol "=")
    tp     <- typeName
    return $ TypeT tName (TDef tp) 

typeType :: Parser DefinedType
typeType = try typeType1 <|> typeType2

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
-- dataCaseList = dataCaseWithArgument `sepBy` bar
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
        ; argumentlist <- typeList
        ; void (symbol ")")
        ; return (cn,argumentlist)})

-- | Parse a list of buildin Type seperate by comma
typeList :: Parser [Type]
typeList = buildinType  `sepBy` comma

--
-- * Parser for Type Declaration
--

-- | Parse the variable name 
--   variable : 1. string of characters, numbers, underscores starting with
--                 an uppercase letter or an underscore
--              2. just '_'
varName :: Parser VarName
varName = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> (upperChar <|> (char '_')) <*> many (alphaNumChar <|> (char '_'))
    check x = if x `elem` reservedwords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

-- | Parse an atom
--   atom: 1. string of characters, numbers, underscores starting with a lowercase letter:
--         2. any single quoted string of characters (not covered)
--         3. numeric literals: 123, -345(didn't cover negarive)
--         4. empty list: [] (didn't cover in this function)
atomName :: Parser AtomName
atomName = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> (lowerChar) <*> many (alphaNumChar <|> (char '_'))
    check x = if x `elem` reservedwords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

-- | Parse an predicate name: start with lower case letter or _ 
predName :: Parser PredName
predName = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> (lowerChar <|> (char '_')) <*> many (alphaNumChar <|> (char '_'))
    check x = if x `elem` reservedwords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x



stringName :: Parser String
stringName = do 
    char '\"'
    str <- identifier 
    char '\"'
    return str

-- | parse the single declaration
typedecl' :: Parser Dec
typedecl' = do 
    reservedword "decl" 
    pn <- predName
    void (symbol "(") 
    tplist <- typeList  
    void (symbol ")")
    return (pn, tplist)

--
-- * Parser for Prolog Program
--

-- | parse a single rule 
rule' :: Parser Rule 
rule' = do 
   hd <- predicateT
   option (Head hd [])(do{
       ; void (symbol ":-")
       ; b <- bodyElemList 
       ; return (Head hd b)})

-- | parse a list of bodyelem
bodyElemList :: Parser [BodyElem]
bodyElemList  = bodyElem `sepBy` comma


-- | parse the argument Name 
argument :: Parser Argument
argument =  Atom <$> atomName
   <|> LitI <$> integer
   <|> Var <$> varName
   <|> LitS <$> stringName
   <|> listNormal
   <|> listVar

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

-- | parse a predicate / single fact
predicateT :: Parser PredicateT
predicateT = do 
    pn <- predName 
    void (symbol "(")
    blist <- bodyElemList 
    void (symbol ")")
    return (Pred pn blist)


argumentList :: Parser [Argument]
argumentList = argument  `sepBy` comma


bodyElem :: Parser BodyElem 
bodyElem =   try isClause
   <|> try (Predicate <$> predicateT)
   <|> ( Arg <$> argument)


-- | Parse isClause such as Y is 4*3
isClause :: Parser BodyElem
isClause = do 
  left <- opExpr
  reservedword "is" 
  right <- opExpr
  return $ Is left right

-- | parse mathmatic clause with its operation
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

opTerm :: Parser BodyElem
opTerm = parens opExpr
  <|> Arg <$> argument

-- | Parse the compare term
compareOp :: Parser Opt 
compareOp = (symbol "=" *> pure Eq)
  <|> (symbol "!=" *> pure Neq)
  <|> (symbol "<" *> pure Lt)
  <|> (symbol "<=" *> pure Leq) 
  <|> (symbol ">=" *> pure Gtq)
  <|> (symbol ">" *> pure Gt)

compareExpr :: Parser BodyElem
compareExpr = do 
  b1 <- opTerm
  op <- compareOp
  b2 <- opTerm
  return $ Oper op b1 b2






