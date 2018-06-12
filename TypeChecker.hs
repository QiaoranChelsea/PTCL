module TypeChecker where

import Text.Megaparsec.Pos
import Types
import ErrorWarTypes
import Errors
import Warnings

----------------------------------------------------------------------------------------------------------------------------------

chcker :: (TypeDef,TypeDic, Prog) -> Report
chcker (typsdef, typdec, prolog ) = R  (errors typsdef typdec prolog  ) (warnings prolog typdec typsdef )

-- combine errors
errors :: TypeDef -> TypeDic -> Prog -> Maybe [Err]
errors f d p = let empty = [] in combineTwoMaybe ( multConErr f, combineTwoMaybe (unknowType d f , combineTwoMaybe (duplicateDef f ,combineTwoMaybe (duplicateDec d, typeErrs p d f))))

-- combine warnings
warnings::  Prog -> TypeDic ->  TypeDef -> Maybe [War]
warnings p d f = case nonDecWarning p d f of
                Nothing -> Nothing
                Just x -> conflict x x
