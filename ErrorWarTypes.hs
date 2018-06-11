module ErrorWarTypes where
    
import Text.Megaparsec.Pos
import Types


-- | The Error Message

data ErrType = TErr | ArrT

data Warining = NonDecl PredFunA Bool
                | Conflict PredFunA PredFunA

data Error = ArgType Dec PredFunA VarMap
            | IncArrit Dec PredFunA VarMap

            | VariableType BodyElem Type VarMap
            | EqType BodyElem Type Type VarMap
            | MissIs  BodyElem
            
            | UnknowType TypeName Dec            
            | MultDec Dec Dec
            | MultDef DefinedType DefinedType
            
type VarMap = (VarTypes,Subsitutions)

type VarType = (VarName,Type)
type VarTypes = [VarType]

type Substitute = (TypeVar,Type)
type Subsitutions = [Substitute]

data Err = E Line Error
data War = W Line Warining

data Report = R (Maybe [Err] ) (Maybe [War])
--
-- * The Domain of PTCL: Check the given Prolog file against type declaration,
--   and provide the report.

type Domain = (TypeDef,TypeDic, Prog) -> Report

---------------------------------------------------------------------------------------------------------------------------------

combineTwoMaybe ::  (Maybe [a], Maybe [a]) -> Maybe [a]
combineTwoMaybe (Just e, Just e')  = Just (e ++ e')
combineTwoMaybe (Just e, _)  = Just e
combineTwoMaybe (_,Just e')  =  Just e'
combineTwoMaybe (_,_)  =  Nothing


decName :: Dec -> TypeName
decName (PredD (n,_)) = n
decName (FuncD (n,_,_)) = n

decTypes :: Dec -> [Type]
decTypes (PredD (_,t)) = t
decTypes (FuncD (_,t,_)) = t

definedTypeName :: DefinedType -> TypeName
definedTypeName (TypeT n _) = n
definedTypeName (DataT n _ _) = n

addToVarMap :: VarType -> VarMap -> VarMap
addToVarMap vt (m,s) = (vt:m, s)


findVar ::  VarName -> VarTypes ->  Maybe VarType
findVar _ [] = Nothing
findVar s (x:xs) = if (varName x) == s then Just x else findVar s xs

varName :: VarType -> VarName
varName = fst

varType :: VarType -> Type
varType = snd


constructorName :: Cons -> ConstructorName
constructorName (n,_) = n

constructorTypes :: Cons -> [Type]
constructorTypes (_,t) = t


getPos :: (a,Line) -> Line
getPos (_,p) = p

getObj :: (a,Line) -> a
getObj (a,_) = a