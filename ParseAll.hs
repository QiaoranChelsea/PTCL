module ParseAll where 

import Parser 
import PTCL

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

parseFromFile p file = runParser p file <$> readFile file

main = parseFromFile parser "sample.pl"

-- data Prolog = DefType [DefinedType]
--             | TypeDel [Dec]
--             | Rules   [Rule]

data Prolog = PL ([DefinedType], [Dec], [Rule])
    deriving (Show)

-- | separate and end by peroid 
definedType :: Parser [DefinedType]
definedType = endBy definedType' period

typedel :: Parser [Dec]
typedel = endBy typedecl' period

rule :: Parser [Rule]
rule = endBy rule' period

parser' :: Parser Prolog 
parser' = do 
    dt <- definedType
    td <- typedel
    rl <- rule
    return $ PL (dt, td, rl)

parser :: Parser Prolog 
parser = between spaceConsumer eof parser'