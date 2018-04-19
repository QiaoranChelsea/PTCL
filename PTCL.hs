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


-- | Type Declaration reoresented by:
--   The Name of the predicate associated with type of its arguments
type Decl = (PredName, [Type])

-- | Names
type PredName = String -- lower case 
type AtomName = String -- lower case 
type VarName = String -- upper case 
type TypeName = String
type ConstructorName = String

-- | Types 
data Constructor = ConstructorName [Type]
data DefinedType = TypeT TypeName Type | DataT TypeName Constructor
data Type = TAtom AtomName | TInt Int | TString String | TList  [Type]  | DefinedType | Var VarName
--
-- * Basic Object of Prolog Program 
--

-- | Predicate in Prolog, Name + Arguments
data PredicateT = Pred PredName [Type] -- at least one element

data Opt = Eq | Neq | And | Lt | Leq | Gt | Gtq | Sub | Add | Div | Mult| Mod 
 
data BodyElem  = Predicate PredicateT
     | Is BodyElem BodyElem  
     | Oper Opt BodyElem BodyElem 
     | Lit Int 
     | Ref VarName

type Body = [BodyElem]

-- | Rules in Prolog, Head + Body 
data Rules = Head PredicateT Body


-- data Object = Mona | Jackie | Marge | Abe | Clancy | Homer

-- female/1
-- female(mona).
-- female(jackie).
-- female(marge).
v1 :: Rules 
v1 = (Head (Pred "female" [TAtom "mona"]) [])

v2 :: Rules 
v2 = (Head (Pred "female" [TAtom "jackie"]) [])

v3 :: Rules 
v3 = (Head (Pred "female" [TAtom "marge"]) [])

-- married_/2
-- married_(abe,mona).
-- married_(clancy,jackie).
-- married_(homer,marge).
f1 :: Rules 
f1 = (Head (Pred "married_" [TAtom "abe", TAtom "mona" ]) [])

f2 :: Rules 
f2 = (Head (Pred "married_" [TAtom "clancy", TAtom "jackie" ]) [])

f3 :: Rules 
f3 = (Head (Pred "married_" [TAtom "homer", TAtom "marge" ]) [])

-- married/2
-- married(X,Y) :- married_(X,Y).
-- married(X,Y) :- married_(Y,X).
g1 :: Rules 
g1 = (Head (Pred "married" [Var "X", Var "Y" ]) [Predicate (Pred "_married" [Var "X", Var "Y" ])])

g2 :: Rules 
g2 = (Head (Pred "married" [Var "X", Var "Y" ]) [Predicate (Pred "_married" [Var "Y", Var "X" ])])

-- eq/2
-- eq(X, Y) :- X = Y.
eq :: Rules 
eq = (Head (Pred "eq" [Var "X", Var "Y" ]) [Oper Eq (Ref "X") (Ref "Y")])

-- double/2
-- double(X, Y) :- Y is X * 2.
double :: Rules 
double = (Head (Pred "double" [Var "X", Var "Y" ]) [Is (Ref "Y") (Oper Mult (Ref "X") (Lit 2))])

prolog :: [Rules]
prolog = [v1 , v2 , v3, f1, f2, f3, g1, g2, eq, double]


typspec :: TypeSpec 
typspec = [ ("married_",  [TAtom "abe", TAtom "mona", 
                             TAtom "clancy", TAtom "jackie", 
                             TAtom "homer", TAtom "marge" ])
          , ("married",  [Var "X", Var "Y" ])
          , ("_married",  [Var "X", Var "Y" ])
          , ("eq", [Var "X", Var "Y"])
          , ("double" ,[Var "X", Var "Y" ])
          ]

domain :: Domain 
domain (prolog,typespec) = (True, "")


