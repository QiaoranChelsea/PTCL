module TypeChecker where

import Types

-- | NOTE:
--   1. ArguDic should store every declared type associated with name.


-- | The Error Message

data ErrType = TErr | ArrT

type Line = Int


data Warining = NonDecl PredFunA Bool
                | Conflict PredFunA PredFunA

data Error = ArgType Dec PredFunA
            | IncArrit Dec PredFunA
            | MultDef DefinedType DefinedType
            | MissIs  BodyElem BodyElem
            | MultDec Dec Dec


-- data Err = E Line Error
-- data War = W Line Warining

data Report = R (Maybe [Error] ) (Maybe [Warining])
--
-- * The Domain of PTCL: Check the given Prolog file against type declaration,
--   and provide the report.

type Domain = (TypeDef,TypeDic, Prog) -> Report


-- ---------------------------------------------------------------------------------------------------------------------------------
--
combineTwoMaybe ::  (Maybe [a], Maybe [a]) -> Maybe [a]
combineTwoMaybe (Just e, Just e')  = Just (e ++ e')
combineTwoMaybe (Just e, _)  = Just e
combineTwoMaybe (_,Just e')  =  Just e'
combineTwoMaybe (_,_)  =  Nothing


checkBody :: (PredFunA -> TypeDic -> TypeDef -> Maybe [a]) -> [BodyElem] -> TypeDic -> TypeDef ->  Maybe [a]
checkBody _ [] d def = Nothing
checkBody f (b:bs) d def = combineTwoMaybe (checkBodyEle f b d def , checkBody f bs d def )

checkBodyEle :: (PredFunA -> TypeDic -> TypeDef -> Maybe [a]) -> BodyElem ->  TypeDic -> TypeDef ->  Maybe [a]
checkBodyEle f (Pred p ) d def = f p d def
checkBodyEle f (And b1 b2) d def = combineTwoMaybe (checkBodyEle f b1 d def , checkBodyEle f b2 d def)
checkBodyEle _ _ _ _ = Nothing

----------------------------------------------------------------------------------------------------------------------------------

chcker :: (TypeDef,TypeDic, Prog) -> Report
chcker (typsdef, typdec, prolog ) = R  (errors typsdef typdec prolog  ) (warnings prolog typdec typsdef )


errors :: TypeDef -> TypeDic -> Prog -> Maybe [Error]
errors f d p = combineTwoMaybe (duplicateDef f ,combineTwoMaybe (duplicateDec d, typeErrs p d f))



warnings::  Prog -> TypeDic ->  TypeDef -> Maybe [Warining]
warnings p d f = case nonDecWarning p d f of
                Nothing -> Nothing
                Just x -> conflict x x

----------------------------------------------------------duplicateDef------------------------------------------------------------------------


duplicateDef :: TypeDef -> Maybe [Error]
duplicateDef [] = Nothing
duplicateDef (x:xs) = combineTwoMaybe(duplicateDef_ x xs, duplicateDef xs)


duplicateDef_ :: DefinedType -> TypeDef -> Maybe [Error]
duplicateDef_ _ [] = Nothing
duplicateDef_ t (x:xs) = if (definedTypeName t == definedTypeName x) then combineTwoMaybe (Just [MultDef t x], duplicateDef_ t xs)
                                                                        else duplicateDef_ t xs
definedTypeName :: DefinedType -> TypeName
definedTypeName (TypeT n _) = n
definedTypeName (DataT n _ _) = n

----------------------------------------------------------duplicateDec------------------------------------------------------------------------

duplicateDec :: TypeDic -> Maybe [Error]
duplicateDec [] = Nothing
duplicateDec (x:xs) = combineTwoMaybe(duplicateDec_ x xs, duplicateDec xs)


duplicateDec_ :: Dec -> TypeDic -> Maybe [Error]
duplicateDec_ _ [] = Nothing
duplicateDec_ t (x:xs) = if (decName t == decName x) then combineTwoMaybe (Just [MultDec t x], duplicateDec_ t xs)
                                                                        else duplicateDec_ t xs
decName :: Dec -> TypeName
decName (PredD (n,_)) = n
decName (FuncD (n,_,_)) = n

decTypes :: Dec -> [Type]
decTypes (PredD (_,t)) = t
decTypes (FuncD (_,t,_)) = t

--------------------------------------------------------ArgType IncArrit------------------------------------------------------------------------

typeErrs :: Prog -> TypeDic ->TypeDef -> Maybe [Error]
typeErrs [] _ _ = Nothing
typeErrs (p:ps) d f = combineTwoMaybe (typeErr p d f , typeErrs ps d f )

typeErr :: Rule -> TypeDic ->TypeDef -> Maybe [Error]
typeErr (Head p b ) d f = combineTwoMaybe (doesMatch p d f  , checkBody doesMatch b d f )


doesMatch :: PredFunA -> TypeDic ->TypeDef -> Maybe [Error]
doesMatch p [] _ =  Nothing
doesMatch p@(n, b ) (d:ds) f =  if n == decName d then combineTwoMaybe( errorType (unifyArgsE b (decTypes d) f) p d, doesMatch p ds f )
                                        else  doesMatch p ds f

-- undefined

errorType :: Maybe ErrType -> PredFunA -> Dec -> Maybe [Error]
errorType Nothing  _ _     = Nothing
errorType (Just TErr) p d  = Just [(ArgType d p )]
errorType (Just ArrT) p d  = Just [(IncArrit d p )]

unifyArgsE ::  [Argument] -> [Type] ->TypeDef ->  Maybe ErrType
unifyArgsE [] [] _         = Nothing
unifyArgsE _ []  _         = Just ArrT
unifyArgsE [] _ _          =  Just ArrT
unifyArgsE (b:bs) (t:ts) f =   case (unifyArgT t b f ) of
                                    True -> (unifyArgsE bs ts f)
                                    False -> (Just TErr)

unifyArgT :: Type -> Argument -> TypeDef ->  Bool
unifyArgT x y f = case x of
                            (TAtom )  ->  unifyAtom y
                            (TInt )   ->  unifyLitI y
                            (TString) ->  unifyLitS y
                            (TList )  ->  unifyList y
                            (TDef n)  -> case findType n f of
                                            Nothing -> False
                                            Just d' -> unifyDefinedType d' y f
                            _         -> True

findType :: TypeName -> TypeDef -> Maybe DefinedType  
findType _ [] = Nothing
findType n (x:xs) = if definedTypeName x == n then Just x else findType n xs 

unifyDefinedType :: DefinedType -> Argument -> TypeDef -> Bool
unifyDefinedType (TypeT _ t ) a f = unifyArgT t a f
unifyDefinedType (DataT _ _ cs ) a f = unifyDef a cs f 

-- type Cons = (ConstructorName,[Type])

unifyDef :: Argument ->  [Cons] -> TypeDef -> Bool
unifyDef (Var _ ) _ _ = True
unifyDef (Atom n ) cs _ = unifyAtomFunc n cs
unifyDef (Func (n,t) ) cs f = case findCon n cs of
                              Nothing -> False
                              Just c  -> unifyFunc (constructorTypes c) t f 
unifyDef _ _ _  = False                       


unifyAtomFunc :: FuncName -> [Cons] -> Bool
unifyAtomFunc _ [] = False
unifyAtomFunc n (x:xs) = case x of 
                              (n, []) -> True
                              _   -> unifyAtomFunc n xs

constructorName :: Cons -> ConstructorName
constructorName (n,_) = n

constructorTypes :: Cons -> [Type]
constructorTypes (_,t) = t
                            
findCon :: String -> [Cons] -> Maybe Cons  
findCon _ [] = Nothing
findCon n (x:xs) = if constructorName x == n then Just x else findCon n xs 


unifyFunc :: [Type] -> [Argument] -> TypeDef -> Bool
unifyFunc [] [] _ = True
unifyFunc _ [] _ = False
unifyFunc [] _ _ = False
unifyFunc (t:ts) (a:as) f =  unifyArgT t a f && unifyFunc ts as f

----------------------------------------------------------nonDec------------------------------------------------------------------------


nonDecWarning :: Prog -> TypeDic -> TypeDef -> Maybe [Warining]
nonDecWarning [] _ _ = Nothing
nonDecWarning (p:ps) d f = combineTwoMaybe (nonDecWarning_ p d f, nonDecWarning ps d f)

nonDecWarning_ :: Rule -> TypeDic -> TypeDef -> Maybe [Warining]
nonDecWarning_ (Head p b ) d f = combineTwoMaybe (doesExist p d f  , checkBody doesExist  b d f )

doesExist :: PredFunA -> TypeDic -> TypeDef  -> Maybe [Warining]
doesExist p [] _ =  Just [NonDecl p False]
doesExist p@(n, _ ) (d:ds)  f = if n == decName d then Nothing
                                            else doesExist p ds f

-------------------------------------------------------Conflict---------------------------------------------------------------------------

conflict :: [Warining] ->  [Warining] -> Maybe [Warining]
conflict  [] ls = Just ls
conflict (x:xs) ls = case doesConflict x ls of
                          Nothing -> conflict xs ls
                          Just y -> conflict xs y

doesConflict :: Warining -> [Warining] -> Maybe [Warining]
doesConflict _ [] = Nothing
doesConflict w@(NonDecl p False) (x:xs) = case x of
                    (NonDecl p' False) -> case unify p p' of
                                            Just y -> combineTwoMaybe (combineTwoMaybe (Just [(NonDecl p' True)], doesConflict w xs) , Just y )
                                            Nothing -> combineTwoMaybe ( Just [x] , doesConflict w xs)
                    _  -> combineTwoMaybe (Just [x], doesConflict w xs)
doesConflict _ ls = Just ls


unify:: PredFunA -> PredFunA -> Maybe [Warining]
unify t1@(n1,b1) t2@(n2, b2) = if (n1 == n2 && (unifyBodies b1 b2 == False)) then (Just [Conflict t1 t2]  )
                                  else Nothing

----------------------------------------------------------unify------------------------------------------------------------------------

unifyBodies :: [Argument] -> [Argument] -> Bool
unifyBodies [] [] = True
unifyBodies (x: xs) (y:ys) =  (unifyArg x y) && (unifyBodies xs ys)

unifyArg :: Argument -> Argument -> Bool
unifyArg x y = case x of
                            (Atom _ ) ->  unifyAtom y
                            (LitI _ ) ->  unifyLitI y
                            (LitS _ ) ->  unifyLitS y
                            (List _ ) ->  unifyList y
                            (Var _ ) -> True
                            (Func _ ) -> True
                            
                            
unifyAtom :: Argument  -> Bool
unifyAtom (Atom _ ) = True
unifyAtom (Var _ ) = True
unifyAtom (Func _ ) = True
unifyAtom _  = False

unifyLitI :: Argument -> Bool
unifyLitI (LitI _ ) = True
unifyLitI (Var _ ) = True
unifyLitI (Func _ ) = True
unifyLitI (OperA _ _ _ ) = True
unifyLitI _  = False

unifyLitS :: Argument -> Bool
unifyLitS (LitS _ ) = True
unifyLitS (Var _ ) = True
unifyLitS (Func _ ) = True
unifyLitS _  = False

unifyList :: Argument -> Bool
unifyList (List _ ) = True
unifyList (Var _ ) = True
unifyList (Func _ ) = True
unifyList _  = False

-- unifyPred :: BodyElem -> BodyElem -> Bool
-- unifyPred  (Predicate _ ) (Predicate _ ) = True
-- unifyPred  (Predicate _ ) _ = False
--
--
--
--
--
--
