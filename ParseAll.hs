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

import TypeChecker
import ErrorWarTypes
import Print
import Errors
import Examples

-- To do: 1. the parameter type in the Type
--        2. bodyElem list ... 
--        3. var .. should we have a type var?
--        4. for type variable, should parser keep ` in the name ?
--        5. Domain analysis

-- parseFromFile p file = runParser p file <$> readFile file


-- split :: (Either (ParseError Char Void) Prolog) -> String  
-- split a@(Right (PL p )) = printReport (chcker p)
-- split b@(Left err)      = show err  

-- runPTCL file  = (split <$> parseFromFile parser file ) >>= putStrLn

-- | separate and end by peroid 
definedTypes :: Parser [(DefinedType, Line)]
definedTypes = endBy definedType period 


-- typedecls :: Parser [(Dec,Line)]
-- typedecls = option ([])$ do 
--     reservedword "decl" 
--     list <- many typedecl 
--     -- list <- endBy typedecl period
--     reservedword "end" <?> "declaration should enclosed between 'decl' and 'end' "
--     return list 

typedecls :: Parser [(Dec,Line)]
typedecls = endBy typedecl period

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










