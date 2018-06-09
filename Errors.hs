module Errors where

import ErrorWarTypes
import Types


checkBodyW :: (PredFunA -> TypeDic -> TypeDef -> VarMap -> (VarMap, Maybe [Error])) -> [BodyElem] -> TypeDic -> TypeDef -> VarMap -> (VarMap, Maybe [Error])
checkBodyW _ [] d def m = (m, Nothing)
checkBodyW f (b:bs) d def m =  let (m', r ) = checkBodyEleW f b d def m in
                               let (m'', r') =  checkBodyW f bs d def m' in  (m'',combineTwoMaybe (r, r'))

checkBodyEleW :: (PredFunA -> TypeDic -> TypeDef -> VarMap -> (VarMap, Maybe [Error])) -> BodyElem ->  TypeDic -> TypeDef -> VarMap -> (VarMap, Maybe [Error])
checkBodyEleW f (Pred p ) d def m = f p d def m
checkBodyEleW f (And b1 b2) d def m = 
                               let (m', r ) = checkBodyEleW f b1 d def m in
                               let (m'', r') =  checkBodyEleW f b2 d def m' in  
                               (m'',combineTwoMaybe (r, r'))                                                           
checkBodyEleW _ b@(Is _ _ )  d def m = isErr b d def m
    -- 
checkBodyEleW _ b@(OperC _ _ _ )  d def m = comErr b d def m 


----------------------------------------------------------duplicateDef----------------------------------------------------------------------

-- find deuplicated def's 
duplicateDef :: TypeDef -> Maybe [Error]
duplicateDef [] = Nothing
duplicateDef (x:xs) = combineTwoMaybe(duplicateDef_ x xs, duplicateDef xs)

duplicateDef_ :: DefinedType -> TypeDef -> Maybe [Error]
duplicateDef_ _ [] = Nothing
duplicateDef_ t (x:xs) = if (definedTypeName t == definedTypeName x) then combineTwoMaybe (Just [MultDef t x], duplicateDef_ t xs)
                                                                        else duplicateDef_ t xs


----------------------------------------------------------duplicateDec----------------------------------------------------------------------

-- find deuplicated dec's
 
duplicateDec :: TypeDic -> Maybe [Error]
duplicateDec [] = Nothing
duplicateDec (x:xs) = combineTwoMaybe(duplicateDec_ x xs, duplicateDec xs)

duplicateDec_ :: Dec -> TypeDic -> Maybe [Error]
duplicateDec_ _ [] = Nothing
duplicateDec_ t (x:xs) = if (decName t == decName x) then combineTwoMaybe (Just [MultDec t x], duplicateDec_ t xs)
                                                                        else duplicateDec_ t xs


------------------------------------------------------Is Oper----------------------------------------------------------------------


-- checkBodyB :: [BodyElem] -> TypeDic -> TypeDef -> VarMap -> (VarMap, Maybe [Error])
-- checkBodyB [] _ _ m = (m, Nothing)
-- checkBodyB (b:bs) d def m =  let (m', r ) = checkBodyEleB b d def m in
--                                let (m'', r') =  checkBodyB bs d def m' in
--                                (m'',combineTwoMaybe (r, r'))

-- checkBodyEleB :: BodyElem ->  TypeDic -> TypeDef -> VarMap -> (VarMap, Maybe [Error])
-- checkBodyEleB b@(Is _ _ ) d def m = isErr b d def m
-- checkBodyEleB b@(OperC _ _ _) d def m = comErr b d def m
-- checkBodyEleB  _ _ _  m = (m, Nothing)


comErr :: BodyElem -> TypeDic -> TypeDef -> VarMap -> (VarMap, Maybe [Error])
comErr b@(OperC Eq l r) _ def m   =  (m, Nothing) -- revise
comErr b@(OperC Neq l r) _ def m  = (m, Nothing) -- revise
comErr b@(OperC o l r) _ def m    = let (m',x) = (unifyArgT TInt l def m) in
                                    let (m'',x') = (unifyArgT TInt r def m') in
                                        case (x && x') of
                                        True ->  (m'', Nothing) 
                                        False ->  (m'', Just([VariableType b TInt m''])) 
                                                                               

-- unifyTwoArgs ::Arguemnt ->  Arguemnt  -> TypeDic -> TypeDef -> VarMap -> (VarMap, Maybe [Error])
-- unifyTwoArgs l r _ def m = findVar
--                           
-- ([("c",TInt)] ++ m,Nothing)
isErr :: BodyElem -> TypeDic -> TypeDef -> VarMap -> (VarMap, Maybe [Error])
isErr b@(Is (LitI _) _) d def m = isErr_ b d def m
isErr b@(Is l@(Var _) _) d def m  =  let (m',x) = unifyArgT TInt l def m in
                                    let ri@(m'', x') = isErr_ b d def m' in
                                    case x of
                                        True -> ri
                                        False ->  (m'', combineTwoMaybe (Just[(VariableType b TInt m'' )], x'))
isErr b@(Is l r) _ def m        = let err = (MissIs b) in
                                  let (m',x) = (unifyArgT TInt l def m) in
                                  let (m'',x') = (unifyArgT TInt r def m') in
                                    case (x && x') of
                                    True ->  (m'', Just [err]) 
                                    False ->  (m'', Just(err:[VariableType b TInt m''])) 
                                    

isErr_ :: BodyElem -> TypeDic -> TypeDef -> VarMap -> (VarMap, Maybe [Error])
isErr_ b@(Is _ r) d def m = let (m',x) = (unifyArgT TInt r def m) in
                                        case x of
                                         True -> (m', Nothing)
                                         False -> (m', Just [VariableType b TInt m'])
                                         
                                         

------------------------------------------------------ArgType IncArrit----------------------------------------------------------------------

-- combine type errors and errities errors
typeErrs :: Prog -> TypeDic ->TypeDef ->  Maybe [Error]
typeErrs [] _ _ = Nothing
typeErrs (p:ps) d f = combineTwoMaybe (typeErr p d f , typeErrs ps d f )

-- type check a rule
typeErr :: Rule -> TypeDic ->TypeDef -> Maybe [Error]
typeErr (Head p b ) d f = 
    let ( m,r)= doesMatch p d f [] in 
    let (m',r') = checkBodyW doesMatch b d f m in 
    combineTwoMaybe (r,r' )



-- find if a predicate or functor matches the type dic 
doesMatch :: PredFunA -> TypeDic ->TypeDef -> VarMap -> (VarMap, Maybe [Error])
doesMatch p [] _ m =  (m, Nothing)
doesMatch p@(n, b ) (d:ds) f m =  
            if n == decName d 
            then let (m', r) = unifyArgsE b (decTypes d) f m in 
                 let (m'',r') = doesMatch p ds f m' in 
                 (m'', (combineTwoMaybe( errorType r  p d m,r' )))
            else  doesMatch p ds f m

-- add the error to the report based on the type of the error
errorType :: Maybe ErrType -> PredFunA -> Dec -> VarMap-> Maybe [Error]
errorType Nothing  _ _   _  = Nothing
errorType (Just TErr) p d m = Just [(ArgType d p m )]
errorType (Just ArrT) p d m= Just [(IncArrit d p m)]

-- unify the arguemtn's list with the type's list 
unifyArgsE ::  [Argument] -> [Type] ->TypeDef -> VarMap ->  (VarMap ,Maybe ErrType)
unifyArgsE [] [] _  m        = (m, Nothing)
unifyArgsE _ []  _  m       = (m, Just ArrT) 
unifyArgsE [] _ _  m      =  (m, Just ArrT) 
unifyArgsE (b:bs) (t:ts) f m =  let (m', r) = (unifyArgT t b f m) in    
                                case r of
                                    True -> (unifyArgsE bs ts f m')
                                    False -> (m',Just TErr)

-- unify one argument with one type
unifyArgT :: Type -> Argument -> TypeDef -> VarMap ->  (VarMap, Bool)
unifyArgT x y f m= case x of
                            (TAtom )  ->  unifyAtom_ y m 
                            (TInt )   ->  unifyLitI_ y m 
                            (TString) ->  unifyLitS_ y m 
                            (TList )  ->  unifyList_ y m 
                            (TDef n)  -> case findType n f of
                                            Nothing -> (m, False)
                                            Just d' -> unifyDefinedType d' y f m
                            _         -> ( m, False)
             
             
unifyAtom_ :: Argument  -> VarMap ->  (VarMap, Bool)
unifyAtom_ (Atom _ ) m = (m, True) 
unifyAtom_ (Var v ) m = unifyArgVar v TAtom m
unifyAtom_ (Func _ ) m = (m, True) 
unifyAtom_ _            m = (m, False)

unifyLitI_ :: Argument -> VarMap ->  (VarMap, Bool)
unifyLitI_ (LitI _ ) m = (m, True) 
unifyLitI_ (Var v ) m = unifyArgVar v TInt m
unifyLitI_ (Func _ ) m = (m, True) 
unifyLitI_ (OperA _ l r ) m = let (m', l') = unifyLitI_ l m in
                              let (m'', r') = unifyLitI_ r m' in
                              (m'',l' && r')                              
unifyLitI_  _            m = (m, False)

unifyLitS_ :: Argument -> VarMap ->  (VarMap, Bool)
unifyLitS_ (LitS _ ) m = (m, True) 
unifyLitS_ (Var v ) m = unifyArgVar v TString m
unifyLitS_ (Func _ ) m = (m, True) 
unifyLitS_ _         m = (m, False)

unifyList_ :: Argument -> VarMap ->  (VarMap, Bool)
unifyList_ (List _ ) m = (m, True) 
unifyList_ (Var v ) m = unifyArgVar v TList m
unifyList_ (Func _ ) m = (m, True) 
unifyList_ _            m = (m, False)

----------------------------------------------------------unify------------------------------------------------------------------------

tVar = "a" 

unifyArgVar :: VarName -> Type -> VarMap -> (VarMap, Bool)
unifyArgVar n t m  = case findVar n m of
                    Nothing -> ((n, t): m, True) 
                    Just x ->  case (varType x) of 
                             (TVar _) -> (replcaceType m n t, True  )
                             t'       -> if  t  == t' then (m, True) else (m, False) 

replcaceType ::  VarMap -> VarName ->Type-> VarMap
replcaceType [] _ _ = []
replcaceType (x:xs) s t = if (varName x) == s then ((s,t):xs) else replcaceType xs s t
          



 ----------------------------------------------------------unify------------------------------------------------------------------------
 
-- find if a type exist in defined types
findType :: TypeName -> TypeDef -> Maybe DefinedType  
findType _ [] = Nothing
findType n (x:xs) = if definedTypeName x == n then Just x else findType n xs 

-- unify argumnet with defined type (data/type)
unifyDefinedType :: DefinedType -> Argument -> TypeDef -> VarMap -> (VarMap, Bool)
unifyDefinedType (TypeT _ t ) a f m = unifyArgT t a f m 
unifyDefinedType (DataT _ _ cs ) a f m = unifyDef a cs f m


-- unify argumnet with defined type constructor
unifyDef :: Argument ->  [Cons] -> TypeDef -> VarMap -> (VarMap, Bool)
unifyDef (Var _ ) _ _ m= (m, True)
unifyDef (Atom n ) cs _ m =  unifyAtomFunc n cs m
unifyDef (Func (n,t) ) cs f m = case findCon n cs of
                              Nothing -> (m, False)
                              Just c  -> unifyFunc (constructorTypes c) t f m
unifyDef _ _ _  m = (m, False)                       


-- unify atom argumnet with empty constructor
unifyAtomFunc :: FuncName -> [Cons] -> VarMap -> (VarMap, Bool)
unifyAtomFunc _ [] m = (m, False) 
unifyAtomFunc n (x:xs) m = case x of 
                              (n, []) -> (m, True) 
                              _   -> unifyAtomFunc n xs m 
             
 -- find constructor       
findCon :: String -> [Cons] -> Maybe Cons  
findCon _ [] = Nothing
findCon n (x:xs) = if constructorName x == n then Just x else findCon n xs 

-- unify a the argument of functor with list of types 
unifyFunc :: [Type] -> [Argument] -> TypeDef ->  VarMap -> (VarMap, Bool)
unifyFunc [] [] _ m = (m, True) 
unifyFunc _ [] _ m = (m, False) 
unifyFunc [] _ _ m = (m, False) 
unifyFunc (t:ts) (a:as) f m =  let (m', r) = unifyArgT t a f m in 
                               let (m'', r') =  unifyFunc ts as f m' in 
                               (m'', r && r')


printMap :: VarMap -> String
printMap [] = "\n"
printMap (x:xs) = show x ++ printMap xs

typeErrP :: Rule -> TypeDic ->TypeDef -> String
typeErrP (Head p b ) d f =
    let ( m,r)= doesMatch p d f [] in 
    let (m',r') = checkBodyW doesMatch b d f m in 
    printMap m'

-- compT :: Type -> Type -> Bool
-- compT TAtom TAtom = True
-- compT TInt TInt = True
-- compT TString TString = True
-- compT TList TList = True
-- compT (TVar x ) (TVar y) = x == y
-- compT (TDef x ) (TDef y) = x == y
-- compT _ _ = False
