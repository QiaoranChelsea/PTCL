module Print where

import Types
import TypeChecker


printReport :: Report -> String
printReport (R err war) = printMaybeErr err ++ printMaybeWaring war

printMaybeErr :: Maybe [Error] -> String
printMaybeErr Nothing = "No errors\n"
printMaybeErr (Just e) = "** Errors **\n" ++ printGList e printError ""


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
printError  (ArgType d p) =  "- Couldnt match expected type "++  printDec d ++ " with " ++ printPredFunType p ++ "\n" ++ "- In the clause " ++ printPredFunVal p ++ "\n"
printError  (IncArrit d p) = "- The predicate for "++  printDec d ++ " expect " ++ show (numArDec d)  ++ " arguments, but " ++  printPredFunType p ++ " has " ++ show (numArPred p)  ++ " arguments.\n" ++ "- In the clause " ++ printPredFunVal p ++ "\n"
printError  (MultDef t1 t2) = "- Multiple definitions of " ++ definedTypeName t1 ++ "\n- Defined at: " ++ printTypeDef t1 ++ " , " ++ printTypeDef t2 ++ "\n"
printError  (MissIs b1 b2) = "MissIs\n"
printError  (MultDec t1 t2) = "- Multiple declaration of " ++ decName t1 ++ "\n- Declared at " ++ printDec t1 ++ " , " ++ printDec t2 ++ "\n"


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
printType (VarT a) = show a

printTypeDef :: DefinedType -> String
printTypeDef (TypeT n t) = "type " ++ n ++ " = " ++ printType t
printTypeDef (DataT n [] ts) = "data " ++ n ++ " = " ++ (printGList ts printCon " | " )
printTypeDef (DataT n v ts) = "data " ++ n ++ " ( " ++ printGList v show "," ++ " )" ++ " = " ++ (printGList ts printCon " | " )


printGList :: [a] -> (a -> String) -> String -> String
printGList [] _ _ = ""
printGList [a] f _ = f a
printGList (x:xs) f s = f x ++ s ++ printGList xs f s

printCon :: Cons -> String
printCon (n , []) = n
printCon (n , ts) = n ++ "(" ++ printGList ts printType "," ++ ")"


printPredFunVal :: PredFunA -> String
printPredFunVal  p = printPredFunc printArgVal p

printPredFunType :: PredFunA -> String
printPredFunType  p =  printPredFunc printArgType p

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


printArgType :: Argument -> String
printArgType (Atom a ) = "atom"
printArgType (LitI a ) =  "int"
printArgType (LitS a ) = "string"
printArgType (List a ) = "list"
printArgType (Var a ) = "var"
printArgType (Func  a ) = printPredFunc printArgType a
printArgType (OperA  o a1 a2 ) = "int" 


--
-- printBodyElems :: (Argument -> String) -> String -> [BodyElem]-> String
-- printBodyElems _ _ [] = ""
-- printBodyElems f _ [x] =  printBodyEle f x
-- printBodyElems f s (x:xs) = printBodyEle f x  ++ s ++ printBodyElems f s xs

-- printBodyEle :: (Argument -> String) -> BodyElem -> String
-- printBodyEle f (Predicate p ) = printPred f p
-- printBodyEle f (Is p1 p2) = printBodyEle f p1 ++ " is " ++ printBodyEle f p2
-- printBodyEle f (Oper op p1 p2) = printBodyEle f p1 ++ printOpt op ++ printBodyEle f p2
-- printBodyEle f (Arg a) = f a





