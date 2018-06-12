module Parser_Lexer where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L 
import qualified Text.Parsec.Token as T

import Types 

type Parser = Parsec Void String

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

--  (*>) :: f a -> f b -> f b
-- | parses the reservedwords and identifiers 
reservedword :: String -> Parser ()
reservedword w = lexeme (string w *> notFollowedBy alphaNumChar)

-- | list of reserved words in scope of type
reservedwords :: [String]
reservedwords = ["atom","int", "list", "stirng", "data", "type", "decl", "end", "is", "var"]


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
-- Names 
-- 

-- | Parse the type name of user defined
typeVarName :: Parser TypeName 
typeVarName = (lexeme . try) (p >>= check >>= checkCapital)
  where
    p       = (:) <$> char '`' <*> many lowerChar
    check x = if x `elem` reservedwords
                then fail $ "type keyword " ++ show x ++ " cannot be an identifier"
                else return x
    checkCapital x = if   length x == 1 
               then fail $ "type variable" ++ " should be lowercase"
               else return x

-- | Parse the type name of user defined
typeUdName :: Parser TypeName 
typeUdName = identifierLower


-- | parse the name of constructor
consName :: Parser TypeName 
consName = identifierLower

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
-- | parse an functor name, it's the same with the predicate
funcName :: Parser PredName
funcName = predName 

-- | Parse a string closed with "
stringName :: Parser String
stringName = do 
    char '\"'
    str <- identifier 
    char '\"'
    return str

