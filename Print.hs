module Print where

import Types
import ErrorWarTypes
import TypeChecker


printReport :: Report -> TypeDef -> Prog -> String
printReport (R err war) def prog = printMaybeErr err def prog ++ printMaybeWaring war 

numArDec :: Dec -> Int
numArDec (PredD (_,b)) = size b
numArDec (FuncD (_,b,_)) = size b

numArPred :: PredFunA -> Int
numArPred (_, b) = size b

size :: [a] -> Int
size [] = 0
size (_:xs) = 1 + size xs

printMaybeWaring :: Maybe [War] -> String
printMaybeWaring  Nothing = "No warnings\n"
printMaybeWaring (Just w ) =  "** Warnings **\n" ++ printGList w printWar ""

printWar :: War -> String
printWar (W pos w)  = "Line " ++ show pos ++ ": " ++  printWaring w  

printWaring :: Warining -> String
printWaring (NonDecl p _ ) = "- Non-Declared: "  ++ printPredFunVal p ++ "\n"
printWaring (Conflict t1 t2)  = "- conflicting argument type: " ++ printPredFunVal t1  ++ " and this " ++ printPredFunVal t2 ++ "\n"


printMaybeErr :: Maybe [Err] -> TypeDef -> Prog -> String
printMaybeErr Nothing _ _ = "No errors\n"
printMaybeErr (Just e) def prog = "** Errors **\n" ++ printGList2 e printErr "" def prog

printErr :: Err -> TypeDef  -> Prog -> String
printErr (E pos e) def prog = "Line " ++ show pos ++ ": " ++  printError e def  ++  printClause pos prog


printClause :: Line -> Prog -> String
printClause l p = case lookup' l p of
                    Nothing -> "\n"
                    Just r -> "- In " ++ printRule r ++ "\n\n"

printRule:: Rule -> String
printRule (Head p []) = printPredFunVal p ++ "."
printRule (Head p b) = printPredFunVal p ++ " :- " ++ printBodyElems ";" b ++ "."


printError :: Error -> TypeDef -> String
printError  (ArgType d p m ) def   =  "- Couldn't match expected type "++  printDec d def m ++ " with " ++ printPredFunType p m def ++ "\n" ++ "- In the clause " ++ printPredFunVal p ++ "\n"
printError  (IncArrit d p m ) def  = "- The predicate for "++  printDec d def m  ++ " expect " ++ show (numArDec d)  ++ " arguments, but " ++  printPredFunType p m def ++ " has " ++ show (numArPred p)  ++ " arguments.\n" ++ "- In the clause " ++ printPredFunVal p ++ "\n"
printError  (MultDef t1 t2 ) def     = "- Multiple definitions of " ++ definedTypeName t1 ++ "\n- Defined at:\n- " ++ printTypeDef t1 def ([],[])++ " \n- " ++ printTypeDef t2 def ([],[]) ++ "\n"
printError  (MultDec t1 t2 m ) def      = "- Multiple declarations of " ++ decName t1 ++ "\n- Declared at\n- " ++ printDec t1 def m  ++ " \n- " ++ printDec t2 def m  ++ "\n"
printError  (MissIs b ) _          = "Misuse of \"is\": expecting Number in the left hand side of \"is\" \n- In " ++  printBodyEle b ++ "\n"
printError  (UnknowType n d m) def     = "Unknown User-Defined-Type \"" ++ n ++ "\" \n- Declared at: " ++ printDec d def m   ++ "\n"
printError  (EqType b t1 t2 m) def   = "The types \"" ++ printType t1 m def ++ "\" and  \"" ++ printType t2 m def ++ "\" do not match \n- In: " ++ printBodyEle b ++ "\n"
printError  (VariableType b t m) def = typeVarErr b t m def

typeVarErr ::  BodyElem -> Type -> VarMap -> TypeDef -> String
typeVarErr b t'([],_)  _ = ""
typeVarErr b t' m@(((v,t):xs),s) def = if t == t' 
                            then typeVarErr b t' (xs,s) def 
                            else "- Couldn't match expected type " ++ printType t' m def ++ " with " ++ v ++ ":" ++ printType t m def ++ "\n- In " ++ printBodyEle b ++ "\n" 

printDec ::  Dec -> TypeDef -> VarMap-> String
printDec (PredD (n, l) ) def m   = n ++ "(" ++ printGListM l printType "," m def ++ ")"
printDec (FuncD (n, l,t) ) def m = n ++ "(" ++ printGListM l printType "," m def ++ ") ->" ++ printType t m def


printType :: Type -> VarMap -> TypeDef -> String
printType (TDef n [] ) _ _ = show n
printType (TDef n t ) _ _ = show n ++ " " ++ printGList t show " "
printType TAtom _ _ = "atom"
printType TString _ _ = "string"
printType TList _ _ = "list"
printType TInt _ _ = "int"
printType (TVar a) m def = printVarInS a m def a

-- ++ "\n"++printMap m'

printTypeDef :: DefinedType  -> TypeDef -> VarMap-> String
printTypeDef (TypeT n t) def m     = "type " ++ n ++ " = " ++ printType t m def  
printTypeDef (DataT n [] ts) def m = "data " ++ n ++ " = " ++ (printGListM ts printCon " | "  m def)
printTypeDef (DataT n v ts) def m  = "data " ++ n ++ " ( " ++ printGList v show "," ++ " )" ++ " = " ++ (printGListM ts printCon " | " m def )


printGList :: [a] -> (a -> String) -> String -> String
printGList [] _ _ = ""
printGList [a] f _ = f a
printGList (x:xs) f s = f x ++ s ++ printGList xs f s

printGList2 :: [a] -> (a -> TypeDef  -> Prog -> String) -> String -> TypeDef  -> Prog -> String
printGList2 [] _ _ _ _          = ""
printGList2 [a] f _ def prog    = f a def prog
printGList2 (x:xs) f s def prog = f x def prog ++ s ++ printGList2 xs f s def prog

printGListM :: [a] -> (a -> VarMap -> TypeDef -> String) -> String -> VarMap -> TypeDef-> String
printGListM [] _ _ _ _ = ""
printGListM [a] f _ m def = f a m def
printGListM (x:xs) f s m def = f x m def ++ s ++ printGListM xs f s m def

printCon :: Cons -> VarMap -> TypeDef -> String
printCon (n , []) _ _ = n
printCon (n , ts) m def = n ++ "(" ++ printGListM ts printType ","  m def ++ ")"


printPredFunVal :: PredFunA -> String
printPredFunVal  p = printPredFunc printArgVal p

printPredFunType :: PredFunA -> VarMap -> TypeDef -> String
printPredFunType  p m def =  printPredFuncM printArgType p m def

printPredFuncM ::  (Argument -> VarMap ->  TypeDef -> String) -> PredFunA -> VarMap -> TypeDef -> String
printPredFuncM f ( n, ts) m def = n ++ "(" ++ (printGListM ts f "," m def )++ ")"

printPredFunc ::  (Argument -> String) -> PredFunA -> String
printPredFunc f ( n, ts) = n ++ "(" ++ (printGList ts f ",")++ ")"

printOptA :: OptA -> String
printOptA Sub = " - "
printOptA Add = " + "
printOptA Div = " / "
printOptA Mult = " * "
printOptA Mod = " % "

printOptC :: OptC -> String
printOptC Eq = " = "
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


printArgType :: Argument  -> VarMap -> TypeDef -> String
printArgType (Atom a ) _  def =  case findCons def a of
                                Just x -> show x
                                Nothing -> "atom"
-- printArgType (Atom a ) _  = "atom"
printArgType (LitI a ) _ _ =  "int"
printArgType (LitS a ) _ _ = "string"
printArgType (List a ) _ _ = "list"
printArgType (Var a ) m'@(m,s) def = printVar a m' def  
                                    
printArgType (Func  a ) m def = printPredFuncM printArgType a m def
printArgType (OperA  o a1 a2 ) _ _ = "int"

printVarInS:: TypeVar -> VarMap -> TypeDef -> TypeVar -> String
printVarInS a m'@(m,s) def va = case findInSub s a  of
                                            Nothing -> va 
                                            (Just (v,t)) -> printType t m' def 
                                    

printVar:: VarName -> VarMap -> TypeDef -> String
printVar a m'@(m,s) def  = case findVar a m of
                            Nothing -> "var"
                            Just x ->  case (varType x) of 
                                    (TVar v) -> case findInSub s v  of
                                            Nothing -> printType (varType x) m' def  
                                            (Just (v,t)) -> printType t m' def 
                                    _ -> printType (varType x) m' def 

findCons :: TypeDef -> AtomName -> Maybe String
findCons [] _ = Nothing
findCons (((DataT name _ cs ),_):xs) n = case findCon n cs of
                                         Nothing -> Nothing
                                         Just _ ->  Just name
findCons _  n = Nothing
    

printBodyElems ::  String -> [BodyElem]-> String
printBodyElems _ [] = ""
printBodyElems _ [x] =  printBodyEle x
printBodyElems s (x:xs) = printBodyEle x  ++ s ++ printBodyElems s xs

printBodyEle ::  BodyElem -> String
printBodyEle (Pred p ) = printPredFunVal p
printBodyEle (Is p1 p2) = printArgVal p1 ++ " is " ++ printArgVal p2
printBodyEle (OperC op p1 p2) = printArgVal p1 ++ printOptC op ++ printArgVal p2
printBodyEle (And  b1 b2) = printBodyEle b1 ++ " , " ++ printBodyEle b2




