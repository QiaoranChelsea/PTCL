module PTCL where

-- To DO: 1. How can we record the user defined type 


-- | The Domain of PTCL: Check the given Prolog file against type declaration,
--   and provide the report.
type Domain = (Prolog, TypeSpec) -> Report 

-- | The output of PTCL is True or False.
--   Should have more detail error message in the future

type Error = String
type Report = (Bool, Error)

-- | A set of Prolog Predicate 
type Prolog = [Rules] 

-- | A list of the type declaration 
type TypeSpec = [Decl]
-- 
-- * Basic Object of Type Declaration
-- 

-- | Type Declaration reoresented by:
--   The Name of the predicate associated with type of its arguments
type Decl = (PredName, [Type])

-- | Name is the name of the predicate 
type PredName = String -- lower case 
-- (Shouldn't be the String here, but don't know what it should be )


-- | Basic Type in Prolog 
data Type = TAtom Object| TInt Int| TString String| TList [Type]
--
-- * Basic Object of Prolog Program 
--

data Object = Mona | Jacki

-- | Predicate in Prolog, Name + Arguments
data PredicateT = Pred PredName [Type] -- at least one element

type VarName = String -- upper case 

data Opt = Eq | Neq | And | Lt | Leq | Gt | Gtq | Sub | Add | Div | Mult| Mod 
 
data BodyElem  = Predicate PredicateT
     | Bind VarName BodyElem  
     | Oper Opt BodyElem BodyElem 
     | Lit Int 
     | Ref VarName

type Body = [BodyElem]

-- | Rules in Prolog, Head + Body 
data Rules = Head PredicateT Body


-- femalMona:: PredicateT
-- femalMona = (Pred "female" [TAtom Mona])




