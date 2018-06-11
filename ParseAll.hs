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
--        3. var .. should we have a type var?



parseFromFile p file = runParser p file <$> readFile file

main = parseFromFile parser "sample.pl"



-- | separate and end by peroid 
definedTypes :: Parser [(DefinedType, Line)]
definedTypes = endBy definedType period 

-- typedecl :: Parser [Dec]
-- typedecl = endBy typedecl' period

typedecls :: Parser [(Dec,Line)]
typedecls = option ([])$ do 
    reservedword "decl" 
    list <- many typedecl 
    -- list <- endBy typedecl period
    reservedword "end" <?> "declaration should enclosed between 'decl' and 'end' "
    return list 

rules :: Parser [(Rule,Line)]
rules = endBy rule period

parser' :: Parser Prolog 
parser' = do 
    dt <- definedTypes
    td <- typedecls
    rl <- rules
    return $ PL (dt, td, rl)

parser :: Parser Prolog 
parser = between spaceConsumer eof parser'










