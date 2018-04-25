module PTCL where

-- | NOTE: 
--   1. ArguDic should store every declared type associated with name.

-- | The Error Message 
type Report = String
-- 
-- * The Domain of PTCL: Check the given Prolog file against type declaration,
--   and provide the report.

type Domain = (TypeDef,TypeDic, Prog) -> Maybe Report 

-- | User defined type
type TypeDef = [DefinedType]
type TypeName = String
type ConstructorName = String

data DefinedType = TypeT TypeName Type | DataT TypeName [(ConstructorName,[Type])]

-- | Types 
data Type = TAtom | TInt | TString| TList | TVar | TDef TypeName 

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

data DefinedTypeValue = TType Arg | TData (ConstructorName,[Arg])

data Arg = AtomT AtomName | IntT Int | StringT String| ListT [Arg]  | VarT VarName | DefT TypeName DefinedTypeValue


-- list :: TypeArg
-- list = ListT [( AtomT "Mona"), (IntT 0 ) ]

-- | Predicate in Prolog, Name + [Type Arguments]
data PredicateT = Pred PredName [Arg] 

-- | Body is a list of BodyElem 
type Body = [BodyElem]

-- | Operation includes Mathematic Operation and comparasion
data Opt = Eq | Neq | And | Lt | Leq | Gt | Gtq | Sub | Add | Div | Mult| Mod 
 
-- | Element of Body: 
data BodyElem  = Predicate PredicateT
     | Is BodyElem BodyElem  
     | Oper Opt BodyElem BodyElem 
     | Lit Int 
     | Ref VarName
--
-- * Examples
--


-- | Define types 

treeType :: DefinedType
treeType = ( DataT "Tree" [("Node", [TInt, TDef "Tree", TDef "Tree" ]),("Leaf", []) ])

nameType :: DefinedType
nameType = ( TypeT "Name" TString )

-- | Declare predicates 

-- predictes_name(type).

d1 :: Dec 
d1 = ("female", [TAtom])

d2 :: Dec 
d2 = ("married_", [TAtom, TAtom])


d3 :: Dec 
d3 = ("married", [TVar, TVar])

d4 :: Dec 
d4 = ("isTree", [TDef "Tree"])

d5 :: Dec 
d5 = ("names", [TDef "Name"])

-- female(mona).
-- female(jacki).
-- female(marge).

v1 :: Rule
v1 = (Head (Pred "female" [AtomT "mona"]) [])

v2 :: Rule
v2 = (Head (Pred "female" [AtomT "jackie"]) [])

v3 :: Rule
v3 = (Head (Pred "female" [AtomT "marge"]) [])


-- | Define predicates 

-- married_/2
-- married_(abe,mona).
-- married_(clancy,jackie).
-- married_(homer,marge).

f1 :: Rule
f1 = (Head (Pred "married_" [AtomT "abe", AtomT "mona" ]) [])

f2 :: Rule
f2 = (Head (Pred "married_" [AtomT "clancy", AtomT "jackie" ]) [])

f3 :: Rule
f3 = (Head (Pred "married_" [AtomT "homer", AtomT "marge" ]) [])

-- married/2
-- married(X,Y) :- married_(X,Y).
-- married(X,Y) :- married_(Y,X).

g1 :: Rule
g1 = (Head (Pred "married" [VarT "X", VarT "Y" ]) [Predicate (Pred "_married" [ VarT "X", VarT "Y" ])])

g2 :: Rule
g2 = (Head (Pred "married" [VarT "X", VarT "Y" ]) [Predicate (Pred "_married" [VarT "Y", VarT "X" ])])

-- eq/2
-- eq(X, Y) :- X = Y.
eq :: Rule
eq = (Head (Pred "eq" [VarT "X", VarT "Y" ]) [Oper Eq (Ref "X") (Ref "Y")])

-- double/2
-- double(X, Y) :- Y is X * 2.
double :: Rule
double = (Head (Pred "double" [VarT "X", VarT "Y" ]) [Is (Ref "Y") (Oper Mult (Ref "X") (Lit 2))])


-- treeVal/1
-- treeVal((Node(3 , Leaf, Leaf) )).

treeVal :: Arg
treeVal = ( DefT "Tree" ( TData ("Node", [ IntT 3,  DefT "Tree" (TData ("Leaf", [] )), DefT "Tree" (TData ("Leaf", [] )) ])))

isTree:: Rule 
isTree = (Head (Pred "isTree" [treeVal ]) [])


-- names/1
-- names("Ghadeer").

nameVal :: Arg
nameVal = ( DefT "Name" ( TType  (StringT "Ghadeer")))

names:: Rule 
names = (Head (Pred "names" [nameVal ]) [])

typsdef :: TypeDef
typsdef = [treeType, nameType]


typspec :: TypeDic
typspec = [ d1, d2, d3]

prolog :: Prog
prolog = [v1 , v2 , v3, f1, f2, f3, g1, g2, eq, double, names, isTree]

domain :: Domain
domain (typsdef, typespec, prolog ) = Nothing




