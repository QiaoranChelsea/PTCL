module RunPTCL where 

import Types

import TypeChecker
import ErrorWarTypes
import Print
import Errors
import Examples

import Parser_Prolog
import Parser_Types
import Parser_Lexer

import ParseAll 


import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr


parseFromFile p file = runParser p file <$> readFile file

split :: (Either (ParseError Char Void) Prolog) -> String  
split a@(Right (PL p@(t,b,c) )) = printReport (checker p) t c 
split b@(Left err)      = parseErrorPretty err  

runPTCL file  = (split <$> parseFromFile parser file ) >>= putStrLn
