module Types where
    
    
-- | Types 
data Type = TAtom | TInt | TString| TList | TDef TypeName  
    deriving(Show)
-- 
-- * User defined type
-- 
type TypeDef = [DefinedType]
type TypeName = String
type ConstructorName = String

-- data DefinedType = TypeT TypeName Type | DataT TypeName [(ConstructorName,[Type])]
--     deriving(Show)

type Cons = (ConstructorName,[Type])

data DefinedType = TypeT TypeName Type | DataT TypeName [Cons]
    deriving(Show)
--
-- * Basic Object of Type Declaration  
--

-- | A list of the type declaration 
-- | Type Declaration represented by:
--   The Name of the predicate associated with type of its arguments
type TypeDic = [Dec]
type Dec = (PredName, [Type])

-- | Names
type VarName = String -- upper case
type AtomName = String -- lower case 
type PredName = String -- lower case 

--
-- * Basic Object of Prolog Program 
--

-- | A set of Prolog Predicate 
type Prog = [Rule] 

-- | Rules in Prolog, Head + Body 
data Rule = Head PredicateT Body
    deriving(Show)
-- data DefinedTypeValue = TypeV Arg | DataV (ConstructorName,[Arg])
    -- deriving(Show)
data Argument = Atom AtomName | LitI Int | LitS String| List [Argument] | Var VarName 
-- | Def TypeName DefinedTypeValue
    deriving(Show)

-- list :: TypeArg
-- list = ListT [( AtomT "Mona"), (IntT 0 ) ]

-- | Predicate in Prolog, Name + [Type Arguments]
data PredicateT = Pred PredName [BodyElem] 
    deriving(Show)
-- | Body is a list of BodyElem 
type Body = [BodyElem]

-- | Operation includes Mathematic Operation and comparasion
data Opt = Eq | Neq | And | Lt | Leq | Gt | Gtq | Sub | Add | Div | Mult| Mod 
    deriving(Show) 
-- | Element of Body: 
data BodyElem  = Predicate PredicateT
     | Is BodyElem BodyElem  
     | Oper Opt BodyElem BodyElem 
     | Arg Argument
     -- | Lit Int 
     -- | Ref VarName
     deriving(Show)