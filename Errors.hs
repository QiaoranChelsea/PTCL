module Errors where

import Text.Megaparsec.Pos
import ErrorWarTypes
import Types


checkBodyErr :: (PredFunA -> Line -> TypeDic -> TypeDef -> VarMap -> (VarMap, Maybe [Err])) -> [BodyElem] -> Line -> TypeDic -> TypeDef -> VarMap -> (VarMap, Maybe [Err])
checkBodyErr _ [] p d def m = (m, Nothing)
checkBodyErr f (b:bs) p d def m =  let (m', r ) = checkBodyEleErr f b p d def m in
                               let (m'', r') =  checkBodyErr f bs p d def m' in  (m'',combineTwoMaybe (r, r'))

checkBodyEleErr :: (PredFunA -> Line -> TypeDic -> TypeDef -> VarMap -> (VarMap, Maybe [Err])) -> BodyElem -> Line ->  TypeDic -> TypeDef -> VarMap -> (VarMap, Maybe [Err])
checkBodyEleErr f (Pred p ) pos d def m = f p pos d def m
checkBodyEleErr _ b@(Is _ _ )  pos d def m = isErr b pos d def m
checkBodyEleErr _ b@(OperC _ _ _ )  pos d def m = comErr b pos d def m
checkBodyEleErr f (And b1 b2) pos d def m =
                               let (m', r ) = checkBodyEleErr f b1 pos d def m in
                               let (m'', r') =  checkBodyEleErr f b2 pos d def m' in
                               (m'',combineTwoMaybe (r, r'))

----------------------------------------------------------duplicateDef----------------------------------------------------------------------

-- find deuplicated def's
unknowType :: TypeDic -> TypeDef -> Maybe [Err]
unknowType [] _ = Nothing
unknowType (x:xs) def = combineTwoMaybe(unknowType_ x def, unknowType xs def)

unknowType_ :: (Dec, Line) -> TypeDef -> Maybe [Err]
unknowType_ p@(d,pos ) def = checktypesList (decTypes d) p def


checktypesList:: [Type] -> (Dec, Line) -> TypeDef -> Maybe[Err]
checktypesList [] _ _ = Nothing
checktypesList (x:xs) p def =  combineTwoMaybe ( checktype x p def ,checktypesList xs p def )


checktype :: Type -> (Dec, Line) -> TypeDef -> Maybe[Err]
checktype  (TDef n _ ) (d, pos) def =  case findType n def of
                                    Nothing -> Just [E pos (UnknowType  n d )]
                                    _ -> Nothing
checktype _ _ _ = Nothing
       

                                                                        
----------------------------------------------------------duplicateDef----------------------------------------------------------------------

-- find deuplicated def's
duplicateDef :: TypeDef -> Maybe [Err]
duplicateDef [] = Nothing
duplicateDef (x:xs) = combineTwoMaybe(duplicateDef_ x xs, duplicateDef xs)

duplicateDef_ :: (DefinedType, Line) -> TypeDef -> Maybe [Err]
duplicateDef_ _ [] = Nothing
duplicateDef_ v@(t,p) ((x,_):xs) = if (definedTypeName t == definedTypeName x) then combineTwoMaybe (Just [E p (MultDef t x) ], duplicateDef_ v xs)
                                                                        else duplicateDef_  v xs

-- ----------------------------------------------------------duplicateDec----------------------------------------------------------------------
--
-- find deuplicated dec's

duplicateDec :: TypeDic -> Maybe [Err]
duplicateDec [] = Nothing
duplicateDec (x:xs) = combineTwoMaybe(duplicateDec_ x xs, duplicateDec xs)

duplicateDec_ :: (Dec, Line) -> TypeDic -> Maybe [Err]
duplicateDec_ _ [] = Nothing
duplicateDec_ v@(t,p) ((x,_):xs) = if (decName t == decName x) then combineTwoMaybe (Just [E p (MultDec t x)], duplicateDec_ v xs)
                                                                        else duplicateDec_ v xs

------------------------------------------------------Is Oper----------------------------------------------------------------------

comErr :: BodyElem -> Line -> TypeDic -> TypeDef -> VarMap -> (VarMap, Maybe [Err])
comErr b@(OperC Eq l r) pos d def m   =  unifyForCom b pos d def m
comErr b@(OperC Neq l r) pos d def m   = unifyForCom b pos d def m                                   
comErr b@(OperC o l r) pos _ def m    = let (m',x) = (unifyArgT TInt l def m) in
                                    let (m'',x') = (unifyArgT TInt r def m') in
                                        case (x && x') of
                                        True ->  (m'', Nothing)
                                        False ->  (m'', Just([E pos (VariableType b TInt m'')]))


unifyForCom :: BodyElem -> Line -> TypeDic -> TypeDef -> VarMap -> (VarMap, Maybe [Err])
unifyForCom b@(OperC _ l r) pos _ def m   =  let (m1, argT1) = argumentType l m in
                                             let (m2, argT2) = argumentType r m1 in
                                                case argT1 of 
                                                    Nothing -> (m1,Just([E pos (VariableType b (TVar tVar) m1)]))
                                                    Just t1 -> case argT2 of 
                                                        Nothing -> (m2,Just([E pos (VariableType b (TVar tVar) m2)]))
                                                        Just t2 -> 
                                                            let (m3,x') = unifyArgT t1 r def m in
                                                            let res = if x' then Nothing else Just([E pos (EqType b t1 t2 m3)]) in
                                                            (m3,res)


isErr :: BodyElem -> Line -> TypeDic -> TypeDef -> VarMap -> (VarMap, Maybe [Err])
isErr b@(Is (LitI _) _) pos d def m = isErr_ b pos d def m
isErr b@(Is l@(Var _) _) pos d def m  =  let (m',x) = unifyArgT TInt l def m in
                                    let ri@(m'', x') = isErr_ b pos d def m' in
                                    case x of
                                        True -> ri
                                        False ->  (m'', combineTwoMaybe (Just[E pos (VariableType b TInt m'' )], x'))
isErr b@(Is l r) pos _ def m        = let err = (E pos (MissIs b)) in
                                  let (m',x) = (unifyArgT TInt l def m) in
                                  let (m'',x') = (unifyArgT TInt r def m') in
                                    case (x && x') of
                                    True ->  (m'', Just [err])
                                    False ->  (m'', Just(err:[E pos (VariableType b TInt m'')]))


isErr_ :: BodyElem -> Line -> TypeDic -> TypeDef -> VarMap -> (VarMap, Maybe [Err])
isErr_ b@(Is _ r) pos d def m = let (m',x) = (unifyArgT TInt r def m) in
                                        case x of
                                         True -> (m', Nothing)
                                         False -> (m', Just [E pos (VariableType b TInt m')])



-- ------------------------------------------------------ArgType IncArrit----------------------------------------------------------------------
--
-- combine type errors and errities errors
typeErrs :: Prog -> TypeDic ->TypeDef ->  Maybe [Err]
typeErrs [] _ _ = Nothing
typeErrs (p:ps) d f = combineTwoMaybe (typeErr p d f , typeErrs ps d f )

-- type check a rule
typeErr :: (Rule, Line) -> TypeDic ->TypeDef -> Maybe [Err]
typeErr ((Head p b ),pos) d f =
    let ( m,r)= doesMatch p pos d f ([],[]) in
    let (m',r') = checkBodyErr doesMatch b pos d f m in
    combineTwoMaybe (r,r' )

-- find if a predicate or functor matches the type dic
doesMatch :: PredFunA -> Line -> TypeDic ->TypeDef -> VarMap -> (VarMap, Maybe [Err])
doesMatch _ _ [] _ m =  (m, Nothing)
doesMatch p@(n, b ) pos ((d,_):ds) f m =
            if n == decName d
            then let (m', r) = unifyArgsE b (decTypes d) f m in
                 let (m'',r') = doesMatch p pos ds f m' in
                 (m'', (combineTwoMaybe( errorType r  p pos d m,r' )))
            else  doesMatch p pos ds f m

-- add the error to the report based on the type of the error
errorType :: Maybe ErrType -> PredFunA -> Line -> Dec -> VarMap-> Maybe [Err]
errorType Nothing  _ _ _   _  = Nothing
errorType (Just TErr) p pos d m = Just [E pos (ArgType d p m )]
errorType (Just ArrT) p pos d m= Just [E pos (IncArrit d p m)]

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
                            (TDef n _ )  -> case findType n f of
                                            Nothing -> (m, False)
                                            Just d' -> unifyDefinedType d' y f m
                            (TVar _)  -> unifyVar_ y x m


unifyVar_ :: Argument  -> Type -> VarMap ->  (VarMap, Bool)
unifyVar_ (Var v ) t m   = unifyArgVar v t m
unifyVar_ (Func _ ) _  m = (m, True)
unifyVar_ (OperA _ l r ) (TVar x) m =
                              let (m', l') = unifyLitI_ l m in
                              let (m'', r') = unifyLitI_ r m' in
                              let (m2, b) = substit m'' x TInt in
                              (m2 ,l' && r' && b)
unifyVar_ a (TVar x) m   = let (m',t) = (argumentType a m) in 
                                case t of 
                                    Just t' -> substit m' x  (t') 
                                    Nothing -> (m',False)

argumentType :: Argument -> VarMap -> (VarMap, Maybe Type)
argumentType  (Atom _ ) m = (m,Just TAtom)
argumentType  (LitI _ ) m = (m,Just TInt) 
argumentType  (LitS _ ) m = (m,Just TString) 
argumentType  (List _ ) m = (m,Just TList) 
argumentType  (Var x )  m = let (m'@(m'',_), _) = unifyArgVar x (TVar tVar) m in 
                            let  res =  findVar x m'' in
                            case res of 
                                Nothing -> (m', Nothing)
                                Just (_,vt) -> (m',Just vt)                            
argumentType  (OperA _ l r ) m = 
                            let (m', l') = unifyLitI_ l m in
                            let (m'', r') = unifyLitI_ r m' in
                            let res = if (l' && r') then Just TInt else Nothing 
                            in (m'',res)
argumentType  (Func _ ) m = (m, Just (TVar tVar))


replcaceByType ::  TypeVar  -> Type -> VarMap -> VarMap
replcaceByType _ _ m@([],s) = m
replcaceByType tv t ((d@(v,(TVar x)):xs) ,s)= let m' = replcaceByType tv t (xs,s) in 
                                                if x ==  tv 
                                                    then addToVarMap (v,t) m' 
                                                    else addToVarMap d m'                               
replcaceByType tv t ((d:xs) , s )=   let m' = replcaceByType tv t (xs,s) in  addToVarMap d m' 
                    

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
unifyArgVar n t m@(m',s)  = case findVar n m' of
                    Nothing -> (addToVarMap (n, t) m, True)
                    Just x ->  case (varType x) of
                             (TVar v) -> substit m v t
                             t'       ->  unifyWithTye t t' m


unifyWithTye :: Type -> Type -> VarMap -> (VarMap, Bool)
unifyWithTye (TVar v) t m@(m',s) = case findInSub s v of 
                                     Nothing -> (m, False)
                                     (Just (v, t') ) -> if  (t  == t)  then (m, True) else (m, False)
unifyWithTye t t' m = if  (t  == t')  then (m, True) else (m, False)

substit:: VarMap -> VarName  -> Type -> (VarMap, Bool)
substit (m,s) v t =  case findInSub s v of 
                         Nothing -> ((m, (v,t):s ), True  )
                         Just (v,vt) -> 
                                 let b = if vt == t then True else False in 
                                  ((m, s ), b  )

findInSub :: Subsitutions -> VarName -> Maybe Substitute
findInSub [] _ = Nothing
findInSub (x:xs) n = if (varName x) == n then Just x else findInSub xs n 

----------------------------------------------------------unify------------------------------------------------------------------------

-- find if a type exist in defined types
findType :: TypeName -> TypeDef -> Maybe (DefinedType,Line)
findType _ [] = Nothing
findType n (v@(x,_):xs) = if definedTypeName x == n then Just v else findType n xs

-- unify argumnet with defined type (data/type)
unifyDefinedType :: (DefinedType,Line) -> Argument -> TypeDef -> VarMap -> (VarMap, Bool)
unifyDefinedType ((TypeT _ t ),_) a f m = unifyArgT t a f m
unifyDefinedType ((DataT _ _ cs ),_) a f m = unifyDef a cs f m


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

----------------------------------------------------------printMap------------------------------------------------------------------------

printMap :: VarMap -> String
printMap (m,s) = printa m ++ "\n" ++ printa s

printa :: Show a => [a] -> String
printa [] = "\n"
printa (x:xs) = show x ++ printa xs

typeErrP ::(Rule, Line) -> TypeDic ->TypeDef  -> String
typeErrP ((Head p b), pos) d f =
    let ( m,r)= doesMatch p pos d f ([],[])  in
    let (m',r') = checkBodyErr doesMatch b pos d f m in
    printMap m'
 ----------------------------------------------------------printMap------------------------------------------------------------------------
    
-- replcaceType ::  VarTypes -> VarName ->Type-> VarTypes
-- replcaceType [] _ _ = []
-- replcaceType (x:xs) n t = if (varName x) == n then ( (n,t): xs) else ( x : (replcaceType xs n t))

--
-- -- compT :: Type -> Type -> Bool
-- -- compT TAtom TAtom = True
-- -- compT TInt TInt = True
-- -- compT TString TString = True
-- -- compT TList TList = True
-- -- compT (TVar x ) (TVar y) = x == y
-- -- compT (TDef x ) (TDef y) = x == y
-- -- compT _ _ = False

-- -- checkBodyB :: [BodyElem] -> TypeDic -> TypeDef -> VarMap -> (VarMap, Maybe [Error])
-- -- checkBodyB [] _ _ m = (m, Nothing)
-- -- checkBodyB (b:bs) d def m =  let (m', r ) = checkBodyEleB b d def m in
-- --                                let (m'', r') =  checkBodyB bs d def m' in
-- --                                (m'',combineTwoMaybe (r, r'))
--
-- -- checkBodyEleB :: BodyElem ->  TypeDic -> TypeDef -> VarMap -> (VarMap, Maybe [Error])
-- -- checkBodyEleB b@(Is _ _ ) d def m = isErr b d def m
-- -- checkBodyEleB b@(OperC _ _ _) d def m = comErr b d def m
-- -- checkBodyEleB  _ _ _  m = (m, Nothing)