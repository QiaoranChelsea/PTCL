module Print where
    
import Types
import TypeChecker


printReport :: Report -> String
printReport (R err war) = printMaybeErr err ++ printMaybeWaring war
    
printMaybeErr :: Maybe [Error] -> String 
printMaybeErr Nothing = "No errors\n"
printMaybeErr (Just e) = "** Errors **\n" ++ printGList e printError "" 


numArDec :: Dec -> Int
numArDec (_,b) = size b

numArPred :: PredicateT -> Int
numArPred (Pred _ b) = size b

size :: [a] -> Int
size [] = 0
size (_:xs) = 1 + size xs


printError :: Error -> String
printError  (ArgType d p) =  "- Couldnt match expected type "++  printDec d ++ " with " ++ printPredType p ++ "\n" ++ " - In the clause " ++ printPredVal p ++ "\n"
printError  (IncArrit d p) = "- The predicate for "++  printDec d ++ " expect " ++ show (numArDec d)  ++ " arguments, but " ++  printPredType p ++ " has " ++ show (numArPred p)  ++ " arguments.\n" ++ "- In the clause " ++ printPredVal p ++ "\n" 
printError  (MultDef t1 t2) = "- Multiple definitions of " ++ definedTypeName t1 ++ "\n- Defined at " ++ printTypeDef t1 ++ " , " ++ printTypeDef t2 ++ "\n" 
printError  (MissIs b1 b2) = "MissIs\n" 
printError  (MultDec t1 t2) = "- Multiple declaration of " ++ decName t1 ++ "\n- Declared at " ++ printDec t1 ++ " , " ++ printDec t2 ++ "\n" 

  
printMaybeWaring :: Maybe [Warining] -> String
printMaybeWaring  Nothing = "No warnings\n"
printMaybeWaring (Just w ) =  "** Warnings **\n" ++ printGList w printWaring ""


printWaring :: Warining -> String
printWaring (NonDecl p _ ) = "- Non-Declared: "  ++ printPredVal p ++ "\n"               
printWaring (Conflict t1 t2)  = "- conflicting argument type: " ++ printPredVal t1  ++ " and this " ++ printPredVal t2 ++ "\n"


printDec ::  Dec -> String
printDec (n, l) = n ++ "(" ++ printGList l printType "," ++ ")" 


printType :: Type -> String
printType (TDef n ) = show n
printType TAtom = "atom"
printType TString = "string"
printType TList = "list"
printType TInt = "int"

printTypeDef :: DefinedType -> String
printTypeDef (TypeT n t) = "type " ++ n ++ " = " ++ printType t 
printTypeDef (DataT n ts) = "data " ++ n ++ " = " ++ (printGList ts printCon " | " ) 

printGList :: [a] -> (a -> String) -> String -> String
printGList [] _ _ = ""
printGList [a] f _ = f a
printGList (x:xs) f s = f x ++ s ++ printGList xs f s

printCon :: Cons -> String
printCon (n , []) = n 
printCon (n , ts) = n ++ "(" ++ printGList ts printType "," ++ ")"

printPredVal:: PredicateT -> String
printPredVal p = printPred printArgVal p

printPredType :: PredicateT -> String
printPredType p =  printPred printArgType p   
    
printPred ::  (Argument -> String) -> PredicateT -> String
printPred f (Pred n ts) = n ++ "(" ++ printBodyElems f ts ++ ")" 


printBodyElems :: (Argument -> String) -> [BodyElem]-> String
printBodyElems _ [] = ""
printBodyElems f [x] =  printBodyEle f x 
printBodyElems f (x:xs) = printBodyEle f x  ++ "," ++ printBodyElems f xs


printOpt :: Opt -> String
printOpt Eq = " == " 
printOpt Neq = " /= " 
printOpt And = " , " 
printOpt Lt = " < " 
printOpt Leq = " <= " 
printOpt Gt = " > " 
printOpt Gtq = " >= " 
printOpt Sub = " - " 
printOpt Add = " + " 
printOpt Div = " / " 
printOpt Mult = " * " 
printOpt Mod = " % " 

printArgVal :: Argument -> String
printArgVal (Atom a ) = show a  
printArgVal (LitI a ) =  show a 
printArgVal (LitS a ) = show a 
printArgVal (List a ) = "[" ++ printGList a printArgVal " , "  ++ "]"
printArgVal (Var a ) = show a 


printArgType :: Argument -> String
printArgType (Atom a ) = "atom"  
printArgType (LitI a ) =  "int" 
printArgType (LitS a ) = "string" 
printArgType (List a ) = "list"  
printArgType (Var a ) = "var"         

printBodyEle :: (Argument -> String) -> BodyElem -> String  
printBodyEle f (Predicate p ) = printPred f p
printBodyEle f (Is p1 p2) = printBodyEle f p1 ++ " is " ++ printBodyEle f p2
printBodyEle f (Oper op p1 p2) = printBodyEle f p1 ++ printOpt op ++ printBodyEle f p2
printBodyEle f (Arg a) = f a 





    