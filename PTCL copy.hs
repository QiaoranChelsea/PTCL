module PTCL where

-- | NOTE: 
--   1. ArguDic should store every declared type associated with name.

-- | The Error Message 
type Report = String
-- 
-- * The Domain of PTCL: Check the given Prolog file against type declaration,
--   and provide the report.

type Domain = (TypeDef,TypeDic, Prog) -> Maybe Report 

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

data DefinedType = TypeT TypeName Type | DataT TypeName [(ConstructorName,[Type])] 
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
data DefinedTypeValue = TypeV Arg | DataV (ConstructorName,[Arg])
    deriving(Show)
data Arg = Atom AtomName | LitI Int | LitS String| List [Arg] | Var VarName | Def TypeName DefinedTypeValue
    deriving(Show)

-- list :: TypeArg
-- list = ListT [( AtomT "Mona"), (IntT 0 ) ]

-- | Predicate in Prolog, Name + [Type Arguments]
data PredicateT = Pred PredName [Arg] 
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
     | Lit Int 
     | Ref VarName
     deriving(Show)

--
-- * Examples
--

-- | Define types 

-- data Tree = Node(Int,Tree,Tree) | Leaf
-- type MyList = List

treeType :: DefinedType
treeType = ( DataT "Tree" [("node", [TInt, TDef "Tree", TDef "Tree" ]),("leaf", []) ])

nameType :: DefinedType
nameType = ( TypeT "MyList" TList )

-- | Declare predicates 

-- predictes_name(type).
-- dec
-- female(Atom)
-- age(Atom, Int)
-- married_(Atom, Atom)
-- married(Var, Var)
-- eq(Var, Var)
-- tree(Tree)
-- isTree(Tree)
-- sumTree(Tree, Int )
-- listLength(MyList, Int)
-- end
--

d1 :: Dec 
d1 = ("female", [TAtom])

d2 :: Dec 
d2 = ("age", [TAtom,TInt])

d3 :: Dec 
d3 = ("married_", [TAtom, TAtom])

d4 :: Dec 
d4 = ("married", [TAtom, TAtom])

d5 :: Dec 
d5 = ("tree", [TDef "Tree"])

d6 :: Dec 
d6 = ("isTree", [TDef "Tree"])

d7 :: Dec 
d7 = ("sumTree", [TDef "Tree", TInt])

d8 :: Dec 
d8 = ("listLength", [TDef "MyList", TInt])


--
-- %%%% valid program
--
-- female(mona).
-- female(jacki).
-- female(marge).
v1 :: Rule
v1 = (Head (Pred "female" [Atom "mona"]) [])

v2 :: Rule
v2 = (Head (Pred "female" [Atom "jackie"]) [])

v3 :: Rule
v3 = (Head (Pred "female" [Atom "marge"]) [])

-- age(mona, 6).
-- age(jacki, 19).
-- age(marge,20).
v4 :: Rule
v4 = (Head (Pred "age" [Atom "mona", LitI 6]) [])

v5 :: Rule
v5 = (Head (Pred "age" [Atom "jacki", LitI 19]) [])

v6 :: Rule
v6 = (Head (Pred "age" [Atom "marge", LitI 20]) [])


-- doubleAge(A,T):-  age(A,Y) , T is Y *2.
v7 :: Rule
v7 = (Head (Pred "doubleAge" [Var "A", Var "T"]) [Oper And (Predicate (Pred "age" [Var "A", Var "Y"]) ) (Is (Ref "T") (Oper Mult (Ref "Y") (Lit 2) )) ])

-- | Define predicates

-- married_(abe,mona).
-- married_(clancy,jackie).
-- married_(homer,marge).

f1 :: Rule
f1 = (Head (Pred "married_" [Atom "abe", Atom "mona" ]) [])

f2 :: Rule
f2 = (Head (Pred "married_" [Atom "clancy", Atom "jackie" ]) [])

f3 :: Rule
f3 = (Head (Pred "married_" [Atom "homer", Atom "marge" ]) [])

-- married(X,Y) :- married_(X,Y).
-- married(X,Y) :- married_(Y,X).
g1 :: Rule
g1 = (Head (Pred "married" [Var "X", Var "Y" ]) [Predicate (Pred "_married" [ Var "X", Var "Y" ])])

g2 :: Rule
g2 = (Head (Pred "married" [Var "X", Var "Y" ]) [Predicate (Pred "_married" [Var "Y", Var "X" ])])

-- eq(X, Y) :- X = Y.
e :: Rule
e = (Head (Pred "eq" [Var "X", Var "Y" ]) [Oper Eq (Ref "X") (Ref "Y")])

-- double(X, Y) :- Y is X * 2.
d :: Rule
d = (Head (Pred "double" [Var "X", Var "Y" ]) [Is (Ref "Y") (Oper Mult (Ref "X") (Lit 2))])


-- tree(leaf).
t1:: Rule
t1 = (Head (Pred "tree" [( Def "Tree" ( DataV ("leaf", [ ])))] ) [])

-- treeVal((node(3 , Leaf, Leaf) )).
t2:: Rule
t2 = (Head (Pred "tree" [( Def "Tree" ( DataV ("node", [ LitI 3,  Def "Tree" (DataV ("leaf", [] )), Def "Tree" (DataV ("leaf", [] )) ]) ) ) ]) [])

-- tree(node(4, leaf, node(3,leaf, node(4,leaf, leaf)))).
t3:: Rule
t3 = (Head (Pred "tree" [( Def "Tree" ( DataV ("node", [ LitI 4,  Def "Tree" (DataV ("leaf", [] )),( Def "Tree" ( DataV ("node", [ LitI 4,  Def "Tree" (DataV ("leaf", [] )), Def "Tree" (DataV ("leaf", [] )) ])))]))) ]) [])

-- isTree(leaf).
-- isTree(node(_, L, R)):- isTree(L),isTree(R).

i1 :: Rule
i1 = (Head (Pred "isTree" [( Def "Tree" ( DataV ("leaf", [ ])))] ) [])

i2 :: Rule
i2 = (Head (Pred "isTree" [( Def "Tree" ( DataV ("node", [ Var "_",  Var "L",  Var "R" ])))]) [Oper And (Predicate (Pred "isTree" [Var "L"]) ) (Predicate (Pred "isTree" [Var "R"]) ) ])


-- sumTree(leaf, 0 ).
-- sumTree(node(I, L, R), T ):- sumTree(L, N1), sumTree(R, N2), T is N1 + N2 + I.

s1:: Rule
s1 = (Head (Pred "sumTree" [( Def "Tree" ( DataV ("leaf", [ ]))) , LitI 0 ] ) [])

s2:: Rule
s2 = (Head (Pred "sumTree" [( Def "Tree" ( DataV ("node", [ Var "_",  Var "L",  Var "R" ]))), Var "T"]) [Oper And (Predicate (Pred "sumTree" [Var "L", Var "N1"]) ) (Oper And  (Predicate (Pred "sumTree" [Var "R", Var "N2"]) ) (Is (Ref "T") (Oper Add (Ref "N1") (Oper Add (Ref "N2") (Ref "T")) ))) ])

-- listLength([], 0).
-- listLength([_|T], Total):-  listLength(T, N) , Total is 1 + N.

l1:: Rule
l1 = (Head (Pred "listLength" [( Def "MyList" ( TypeV ( List []))) , LitI 0 ] ) [])

l2:: Rule
l2 = (Head (Pred "listLength" [( Def "MyList" ( TypeV (List ((Var "_"):[(Var "T")])))), Var "Total" ]) [Oper And (Predicate (Pred "listLength" [Var "T", Var "N"]) ) (Is (Ref "Total") (Oper Add (Lit 1) (Ref "N"))) ])


typsdef :: TypeDef
typsdef = [treeType, nameType]


typspec :: TypeDic
typspec = [ d1, d2, d3, d4, d5, d6, d7 , d8 ]

prolog :: Prog
prolog = [v1, v2, v3, v4, v5, v6, v7, f1, f2, f3, g1, g2, e, d, t1, t2, t3, i1, t2, s1, s2, l1, l2]

domain :: Domain
domain (typsdef, typespec, prolog ) = Nothing




