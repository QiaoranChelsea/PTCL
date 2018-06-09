module ParseAll where 



import Parser_Prolog
import Parser_Types
import Parser_Lexer
import Types


import Control.Applicative (empty)
import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


-- To do: 1. the parameter type in the Type
--        2. bodyElem list ... 



parseFromFile p file = runParser p file <$> readFile file

main = parseFromFile parser "sample.pl"



-- | separate and end by peroid 
definedTypes :: Parser [(DefinedType, SourcePos)]
definedTypes = endBy definedType period

-- typedecl :: Parser [Dec]
-- typedecl = endBy typedecl' period

typedecls :: Parser [(Dec,SourcePos)]
typedecls = do 
    reservedword "decl" 
    list <- many typedecl
    reservedword "end"
    return list 

rules :: Parser [(Rule,SourcePos)]
rules = endBy rule period

parser' :: Parser Prolog 
parser' = do 
    dt <- definedTypes
    td <- typedecls
    rl <- rules
    return $ PL (dt, td, rl)

parser :: Parser Prolog 
parser = between spaceConsumer eof parser'










