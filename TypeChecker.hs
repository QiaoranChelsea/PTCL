module TypeChecker where

import Text.Megaparsec.Pos
import Types
import ErrorWarTypes
import Errors
import Warnings

----------------------------------------------------------------------------------------------------------------------------------

checker :: (TypeDef,TypeDic, Prog) -> Report
checker (typsdef, typdec, prolog ) = R  (errors typsdef typdec prolog  ) (warnings prolog typdec typsdef )

-- combine errors from  duplicateDec and type and arrities errors
errors :: TypeDef -> TypeDic -> Prog -> Maybe [Err]
errors f d p = let empty = [] in combineTwoMaybe (unknowType d f , combineTwoMaybe (duplicateDef f ,combineTwoMaybe (duplicateDec d, typeErrs p d f)))


-- takes the list of non declared predicates and find the conflict in them
warnings::  Prog -> TypeDic ->  TypeDef -> Maybe [War]
warnings p d f = case nonDecWarning p d f of
                Nothing -> Nothing
                Just x -> conflict x x

--
-- unifyPred :: BodyElem -> BodyElem -> Bool
-- unifyPred  (Predicate _ ) (Predicate _ ) = True
-- unifyPred  (Predicate _ ) _ = False
--
