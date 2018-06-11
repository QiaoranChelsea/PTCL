module Warnings where

import Text.Megaparsec.Pos
import ErrorWarTypes
import Types

checkBodyWar :: (PredFunA -> Line -> TypeDic -> TypeDef -> Maybe [a]) -> [BodyElem] -> Line -> TypeDic -> TypeDef ->  Maybe [a]
checkBodyWar _ [] _ d def = Nothing
checkBodyWar f (b:bs) pos d def = combineTwoMaybe (checkBodyEleWar f b pos d def , checkBodyWar f bs pos d def )

checkBodyEleWar :: (PredFunA -> Line -> TypeDic -> TypeDef -> Maybe [a]) -> BodyElem -> Line->  TypeDic -> TypeDef ->  Maybe [a]
checkBodyEleWar f (Pred p ) pos d def = f p pos d def
checkBodyEleWar f (And b1 b2) pos d def = combineTwoMaybe (checkBodyEleWar f b1 pos d def , checkBodyEleWar f b2 pos d def)
checkBodyEleWar _ _ _ _ _ = Nothing

----------------------------------------------------------nonDec------------------------------------------------------------------------

-- find non declared predicates
nonDecWarning :: Prog -> TypeDic -> TypeDef -> Maybe [War]
nonDecWarning [] _ _ = Nothing
nonDecWarning (p:ps) d f = combineTwoMaybe (nonDecWarning_ p d f, nonDecWarning ps d f)

-- find non declared predicates in rule
nonDecWarning_ :: (Rule,Line) -> TypeDic -> TypeDef -> Maybe [War]
nonDecWarning_ ((Head p b ),pos) d f = combineTwoMaybe (doesExist p pos d f , checkBodyWar doesExist b  pos d f )

-- find if this pred exist in dec list
doesExist :: PredFunA -> Line -> TypeDic -> TypeDef  -> Maybe [War]
doesExist p pos [] _  =  Just [W pos (NonDecl p False)]
doesExist p@(n, _ ) pos ((d,_):ds) f = if n == decName d then Nothing
                                            else doesExist p pos ds f
                                            
-------------------------------------------------------Conflict---------------------------------------------------------------------------
                                            
conflict :: [War] ->  [War] -> Maybe [War]
conflict  [] ls = Just ls
conflict (x:xs) ls = case doesConflict x ls of
                          Nothing -> conflict xs ls
                          Just y -> conflict xs y

doesConflict :: War -> [War] -> Maybe [War]
doesConflict _ [] = Nothing
doesConflict w@(W pos (NonDecl p False)) (x:xs) = case x of
                    (W pos' (NonDecl p' False)) -> case unify (p,pos) (p',pos') of
                                            Just y -> combineTwoMaybe (combineTwoMaybe (Just [W pos' (NonDecl p' True)], doesConflict w xs) , Just y )
                                            Nothing -> combineTwoMaybe ( Just [x] , doesConflict w xs)
                    _  -> combineTwoMaybe (Just [x], doesConflict w xs)
doesConflict _ ls = Just ls


unify:: (PredFunA,Line) -> (PredFunA,Line) -> Maybe [War]
unify (t1@(n1,b1),pos1) (t2@(n2, b2),pos2) = if (n1 == n2 && (unifyBodies b1 b2 == False)) then (Just [(W pos2 (Conflict t1 t2))]  )
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



-- checkBody :: (PredFunA -> Line -> TypeDic -> TypeDef -> VarMap -> (VarMap, Maybe [a])) -> [BodyElem] -> Line -> TypeDic -> TypeDef -> VarMap -> (VarMap, Maybe [a])
-- checkBody _ [] _ d def m = (m,Nothing)
-- checkBody f (b:bs) pos d def m =
--     let (m',r') = checkBodyEle f b pos d def m in
--     let (m'',r'') = checkBody f bs pos d def m' in
--      (m'',combineTwoMaybe (r' , r''))
--
-- checkBodyEle :: (PredFunA -> Line -> TypeDic -> TypeDef -> VarMap -> (VarMap, Maybe [a])) -> BodyElem -> Line->  TypeDic -> TypeDef -> VarMap -> (VarMap, Maybe [a])
-- checkBodyEle f (Pred p ) pos d def m = f p pos d def m
-- checkBodyEle f (And b1 b2) pos d def m =
--     let (m',r') = checkBodyEle f b1 pos d def m in
--     let (m'',r'') = checkBodyEle f b2 pos d def m' in
--      (m'',combineTwoMaybe (r' , r''))
-- checkBodyEle _ _ _ _ _ m = (m,Nothing)
--
--
-- -- ----------------------------------------------------------nonDec------------------------------------------------------------------------
--
-- -- find non declared predicates
-- nonDecWarning :: Prog -> TypeDic -> TypeDef -> Maybe [War]
-- nonDecWarning [] _ _ = Nothing
-- nonDecWarning (p:ps) d f = combineTwoMaybe (nonDecWarning_ p d f, nonDecWarning ps d f)
--
-- -- find non declared predicates in rule
-- nonDecWarning_ :: (Rule,Line) -> TypeDic -> TypeDef -> Maybe [War]
-- nonDecWarning_ ((Head p b ),pos) d f =
--     let (m',r') = doesExist p pos d f [] in
--     let (_,r'') = checkBody doesExist b  pos d f m' in
--      combineTwoMaybe (r' , r'')
--
--
-- -- find if this pred exist in dec list
-- doesExist :: PredFunA -> Line -> TypeDic -> TypeDef  -> VarMap -> (VarMap, Maybe [War])
-- doesExist p pos [] _  m =  (m, Just [W pos (NonDecl p False)])
-- doesExist p@(n, _ ) pos ((d,_):ds) f m = if n == decName d then (m,Nothing)
--                                             else doesExist p pos ds f m


