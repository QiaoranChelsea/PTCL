module Print where

import Types
import ErrorWarTypes
import TypeChecker



printReport :: Report -> String
printReport (R err war) = printMaybeErr err ++ printMaybeWaring war

printMaybeErr :: Maybe [Error] -> String
printMaybeErr Nothing = "No errors\n"
printMaybeErr (Just e) = "** Errors **\n\n" ++ printGList e printError ""


numArDec :: Dec -> Int
numArDec (PredD (_,b)) = size b
numArDec (FuncD (_,b,_)) = size b

numArPred :: PredFunA -> Int
numArPred (_, b) = size b

size :: [a] -> Int
size [] = 0
size (_:xs) = 1 + size xs

-- --
-- -- -- p(X,Y) :- d(X,Y); d(Y,X)
--
printError :: Error -> String
printError  (ArgType d p m ) =  "- Couldnt match expected type "++  printDec d ++ " with " ++ printPredFunType p m ++ "\n" ++ "- In the clause " ++ printPredFunVal p ++ "\n\n"
printError  (IncArrit d p m ) = "- The predicate for "++  printDec d ++ " expect " ++ show (numArDec d)  ++ " arguments, but " ++  printPredFunType p m ++ " has " ++ show (numArPred p)  ++ " arguments.\n" ++ "- In the clause " ++ printPredFunVal p ++ "\n\n"
printError  (MultDef t1 t2 ) = "- Multiple definitions of " ++ definedTypeName t1 ++ "\n- Defined at: " ++ printTypeDef t1 ++ " , " ++ printTypeDef t2 ++ "\n\n"
printError  (MultDec t1 t2) = "- Multiple declaration of " ++ decName t1 ++ "\n- Declared at " ++ printDec t1 ++ " , " ++ printDec t2 ++ "\n\n"
printError  (MissIs b ) = "Misuse of \"is\": expecting Number in the left hand side of \"is\" in " ++  printBodyEle b ++ "\n\n"

 -- in "5+3 is 8"
printError  (VariableType b t m) = "VariableType\n\n"



printMaybeWaring :: Maybe [Warining] -> String
printMaybeWaring  Nothing = "No warnings\n"
printMaybeWaring (Just w ) =  "** Warnings **\n" ++ printGList w printWaring ""


printWaring :: Warining -> String
printWaring (NonDecl p _ ) = "- Non-Declared: "  ++ printPredFunVal p ++ "\n"
printWaring (Conflict t1 t2)  = "- conflicting argument type: " ++ printPredFunVal t1  ++ " and this " ++ printPredFunVal t2 ++ "\n"


printDec ::  Dec -> String
printDec (PredD (n, l) ) = n ++ "(" ++ printGList l printType "," ++ ")"
printDec (FuncD (n, l,t) ) = n ++ "(" ++ printGList l printType "," ++ ") ->" ++ printType t


printType :: Type -> String
printType (TDef n ) = show n
printType TAtom = "atom"
printType TString = "string"
printType TList = "list"
printType TInt = "int"
printType (TVar a) = show a

printTypeDef :: DefinedType -> String
printTypeDef (TypeT n t) = "type " ++ n ++ " = " ++ printType t
printTypeDef (DataT n [] ts) = "data " ++ n ++ " = " ++ (printGList ts printCon " | " )
printTypeDef (DataT n v ts) = "data " ++ n ++ " ( " ++ printGList v show "," ++ " )" ++ " = " ++ (printGList ts printCon " | " )


printGList :: [a] -> (a -> String) -> String -> String
printGList [] _ _ = ""
printGList [a] f _ = f a
printGList (x:xs) f s = f x ++ s ++ printGList xs f s

printGListM :: [a] -> (a -> VarMap -> String) -> String -> VarMap -> String
printGListM [] _ _ _ = ""
printGListM [a] f _ m = f a m
printGListM (x:xs) f s m = f x m ++ s ++ printGListM xs f s m

printCon :: Cons -> String
printCon (n , []) = n
printCon (n , ts) = n ++ "(" ++ printGList ts printType "," ++ ")"


printPredFunVal :: PredFunA -> String
printPredFunVal  p = printPredFunc printArgVal p

printPredFunType :: PredFunA -> VarMap -> String
printPredFunType  p m =  printPredFuncM printArgType p m 

printPredFuncM ::  (Argument -> VarMap -> String) -> PredFunA -> VarMap -> String
printPredFuncM f ( n, ts) m = n ++ "(" ++ (printGListM ts f "," m)++ ")"

printPredFunc ::  (Argument -> String) -> PredFunA -> String
printPredFunc f ( n, ts) = n ++ "(" ++ (printGList ts f ",")++ ")"

printOptA :: OptA -> String
printOptA Sub = " - "
printOptA Add = " + "
printOptA Div = " / "
printOptA Mult = " * "
printOptA Mod = " % "

printOptC :: OptC -> String
printOptC Eq = " == "
printOptC Neq = " /= "
printOptC Lt = " < "
printOptC Leq = " <= "
printOptC Gt = " > "
printOptC Gtq = " >= "


printArgVal :: Argument -> String
printArgVal (Atom a ) = show a
printArgVal (LitI a ) =  show a
printArgVal (LitS a ) = show a
printArgVal (List a ) = "[" ++ printGList a printArgVal " , "  ++ "]"
printArgVal (Var a ) = show a
printArgVal (Func  a ) = printPredFunc printArgVal a
printArgVal (OperA  o a1 a2 ) = printArgVal a1 ++ printOptA o ++ printArgVal a2 


printArgType :: Argument -> VarMap -> String
printArgType (Atom a ) _  = "atom"
printArgType (LitI a ) _ =  "int"
printArgType (LitS a ) _ = "string"
printArgType (List a ) _ = "list"
printArgType (Var a ) m = case findVar a m of
                            Nothing -> "var"
                            Just x ->  printType (varType x) 
printArgType (Func  a ) m = printPredFuncM printArgType a m
printArgType (OperA  o a1 a2 ) _ = "int" 




printBodyElems ::  String -> [BodyElem]-> String
printBodyElems _ [] = ""
printBodyElems _ [x] =  printBodyEle x
printBodyElems s (x:xs) = printBodyEle x  ++ s ++ printBodyElems s xs

printBodyEle ::  BodyElem -> String
printBodyEle (Pred p ) = printPredFunVal p
printBodyEle (Is p1 p2) = printArgVal p1 ++ " is " ++ printArgVal p2
printBodyEle (OperC op p1 p2) = printArgVal p1 ++ printOptC op ++ printArgVal p2
printBodyEle (And  b1 b2) = printBodyEle b1 ++ " , " ++ printBodyEle b2







