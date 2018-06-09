module Parser_Types where

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



-- 
-- Parser for Build-in Type and UserDefined Types
--


buildinType :: Parser Type
buildinType = TAtom <$ reservedword "atom"
    <|> TInt <$ reservedword "int"
    <|> TString <$ reservedword "string"
    <|> TList <$ reservedword "list"
    <|> TDef <$> identifier


--
-- Parser for User Defined Type
--
definedType :: Parser (DefinedType,SourcePos)
definedType = do 
    dt  <- definedType'
    pos <- getPosition
    return (dt,pos)

-- | parser for user defined type
definedType' :: Parser DefinedType
definedType' = typeType 
    <|> dataType

-- | parser for type constructor
typeType :: Parser DefinedType
typeType = try typeType1 <|> typeType2

typeType1 :: Parser DefinedType 
typeType1 =  do 
    reservedword "type"
    tName  <- typeUdName
    void (symbol "=")
    tp     <- buildinType
    return $ TypeT tName tp

typeType2 :: Parser DefinedType 
typeType2 =  do 
    reservedword "type"
    tName  <- typeUdName
    void (symbol "=")
    tp     <- typeUdName
    return $ TypeT tName (TDef tp) 


-- | Parser for data type 
dataType :: Parser DefinedType
dataType = try dataTypeWithParameter <|> dataTypeWithoutParameter

dataTypeWithoutParameter :: Parser DefinedType
dataTypeWithoutParameter =  do 
    reservedword "data"
    tyname <- typeUdName
    void (symbol "=")
    dlist  <- dataCaseList
    return $ DataT tyname [] dlist 

dataTypeWithParameter :: Parser DefinedType
dataTypeWithParameter =  do 
    reservedword "data"
    tyname <- typeUdName
    vname  <- many typeVarName
    void (symbol "=")
    dlist  <- dataCaseList
    return $ DataT tyname vname dlist 

dataCaseList :: Parser [(ConstructorName,[Type])]
dataCaseList = (dataCase) `sepBy` bar

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
typedecl :: Parser (Dec,SourcePos)
typedecl = do 
    td  <- typedecl'
    pos <- getPosition
    return (td,pos)

-- | parse the single declaration
typedecl' :: Parser Dec
typedecl' = try funcTypeDecl <|>  predTypeDecl

-- | parse the type declaration of predicate
predTypeDecl :: Parser Dec 
predTypeDecl =  do 
    -- reservedword "decl" 
    pn <- predName
    void (symbol "(") 
    tplist <- typeList  
    void (symbol ")")
    void (symbol ".")
    return (PredD (pn, tplist))

-- | parse the type declaration of functor which has the return type
funcTypeDecl :: Parser Dec 
funcTypeDecl = do 
    -- reservedword "decl" 
    pn <- funcName
    void (symbol "(") 
    tplist <- typeList  
    void (symbol ")")
    void (symbol "->")
    rtlist <- buildinType
    void (symbol ".")
    return (FuncD (pn, tplist,rtlist))









