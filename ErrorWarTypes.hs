module ErrorWarTypes where
    
import Text.Megaparsec.Pos
import Types


-- | The Error report

data ErrType = TErr | ArrT

data Warining = NonDecl PredFunA Bool
                | Conflict PredFunA PredFunA

data Error = ArgType Dec PredFunA VarMap
            | IncArrit Dec PredFunA VarMap

            | VariableType BodyElem Type VarMap
            | EqType BodyElem Type Type VarMap
            | MissIs  BodyElem
            
            | UnknowType TypeName Dec VarMap          
            | MultDec Dec Dec VarMap
            | MultCon ConstructorName DefinedType DefinedType
            | MultDef DefinedType DefinedType
            
-- the map has variable types and subsitutions
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

emptyMap = ([],[])

-- combine two lists of maybe
combineTwoMaybe ::  (Maybe [a], Maybe [a]) -> Maybe [a]
combineTwoMaybe (Just e, Just e')  = Just (e ++ e')
combineTwoMaybe (Just e, _)  = Just e
combineTwoMaybe (_,Just e')  =  Just e'
combineTwoMaybe (_,_)  =  Nothing

-- takes a dec of functor or pred and return the name of it
decName :: Dec -> TypeName
decName (PredD (n,_)) = n
decName (FuncD (n,_,_)) = n

-- takes a dec of functor or pred and return the argument type list 
decTypes :: Dec -> [Type]
decTypes (PredD (_,t)) = t
decTypes (FuncD (_,t,_)) = t

-- takes the def of data type of user defined type and return its name 
definedTypeName :: DefinedType -> TypeName
definedTypeName (TypeT n _) = n
definedTypeName (DataT n _ _) = n

-- find if a type exist in defined types
findType :: TypeName -> TypeDef -> Maybe (DefinedType,Line)
findType _ [] = Nothing
findType n (v@(x,_):xs) = if definedTypeName x == n then Just v else findType n xs

-- find if there is a substitution for a type var
findInSub :: Subsitutions -> VarName -> Maybe Substitute
findInSub [] _ = Nothing
findInSub (x:xs) n = if (varName x) == n then Just x else findInSub xs n 

 -- find constructor in a list of constuctors 
findCon :: String -> [Cons] -> Maybe Cons
findCon _ [] = Nothing
findCon n (x:xs) = if constructorName x == n then Just x else findCon n xs

 -- add a varibale to the variable types in map 
addToVarMap :: VarType -> VarMap -> VarMap
addToVarMap vt (m,s) = (vt:m, s)

 -- find a varibale in the variable types list 
findVar ::  VarName -> VarTypes ->  Maybe VarType
findVar _ [] = Nothing
findVar s (x:xs) = if (varName x) == s then Just x else findVar s xs

-- findCons :: TypeDef -> ConstructorName -> Maybe DefinedType
-- findCons [] _ = Nothing
-- findCons ((t@(DataT name _ cs ),_):xs) n = case findCon n cs of
--                                          Nothing -> Nothing
--                                          Just t ->  Just t
-- findCons _  n = Nothing

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

printMap :: VarMap -> String
printMap (m,s) = printa m ++ "\n" ++ printa s

printa :: Show a => [a] -> String
printa [] = "\n"
printa (x:xs) = show x ++ printa xs

-- find the rule that has this line number 
lookup' :: (Eq b) => b -> [(a,b)] -> Maybe a
lookup' _ [] =  Nothing
lookup' key ((x,y):xys)
  | key == y  =  Just x
  | otherwise =  lookup' key xys
  
size :: [a] -> Int
size [] = 0
size (_:xs) = 1 + size xs

numArDec :: Dec -> Int
numArDec (PredD (_,b)) = size b
numArDec (FuncD (_,b,_)) = size b

numArPred :: PredFunA -> Int
numArPred (_, b) = size b  