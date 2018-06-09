module Warnings where

import ErrorWarTypes
import Types

checkBody :: (PredFunA -> TypeDic -> TypeDef -> Maybe [a]) -> [BodyElem] -> TypeDic -> TypeDef ->  Maybe [a]
checkBody _ [] d def = Nothing
checkBody f (b:bs) d def = combineTwoMaybe (checkBodyEle f b d def , checkBody f bs d def )

checkBodyEle :: (PredFunA -> TypeDic -> TypeDef -> Maybe [a]) -> BodyElem ->  TypeDic -> TypeDef ->  Maybe [a]
checkBodyEle f (Pred p ) d def = f p d def
checkBodyEle f (And b1 b2) d def = combineTwoMaybe (checkBodyEle f b1 d def , checkBodyEle f b2 d def)
checkBodyEle _ _ _ _ = Nothing



----------------------------------------------------------nonDec------------------------------------------------------------------------

-- find non declared predicates 
nonDecWarning :: Prog -> TypeDic -> TypeDef -> Maybe [Warining]
nonDecWarning [] _ _ = Nothing
nonDecWarning (p:ps) d f = combineTwoMaybe (nonDecWarning_ p d f, nonDecWarning ps d f)

-- find non declared predicates in rule
nonDecWarning_ :: Rule -> TypeDic -> TypeDef -> Maybe [Warining]
nonDecWarning_ (Head p b ) d f = combineTwoMaybe (doesExist p d f  , checkBody doesExist  b d f )

-- find if this pred exist in dec list 
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

unifyBodies :: [Argument] -> [Argument] -> Bool
unifyBodies [] [] = True
unifyBodies (x: xs) (y:ys) =  (unifyArg x y) && (unifyBodies xs ys)



----------------------------------------------------------------------------------------------------------------------------------

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