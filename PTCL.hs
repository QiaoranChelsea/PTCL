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
v1 = (Head (Pred "female" [Arg (Atom "mona")]) [])

v2 :: Rule
v2 = (Head (Pred "female" [Arg (Atom "jackie")]) [])

v3 :: Rule
v3 = (Head (Pred "female" [Arg (Atom "marge")]) [])


-- age(mona, 6).
-- age(jacki, 19).
-- -- age(marge,20).
v4 :: Rule
v4 = (Head (Pred "age" [Arg (Atom "mona"), Arg (LitI 6)]) [])

v5 :: Rule
v5 = (Head (Pred "age" [Arg ( Atom "jacki"),Arg ( LitI 19)]) [])

v6 :: Rule
v6 = (Head (Pred "age" [Arg (Atom "marge"), Arg (LitI 20)]) [])


-- -- doubleAge(A,T):-  age(A,Y) , T is Y *2.
v7 :: Rule
v7 = (Head (Pred "doubleAge" [Arg ( Var "A"), Arg (Var "T")]) [Oper And (Predicate (Pred "age" [Arg (Var "A"), Arg (Var "Y")]) ) (Is (Arg (Var "T")) (Oper Mult (Arg (Var "Y")) (Arg (LitI 2)) )) ])


-- | Define predicates

-- married_(abe,mona).
-- married_(clancy,jackie).
-- married_(homer,marge).

f1 :: Rule
f1 = (Head (Pred "married_" [Arg (Atom "abe"), Arg (Atom "mona" )]) [])

f2 :: Rule
f2 = (Head (Pred "married_" [Arg (Atom "clancy"), Arg (Atom "jackie") ]) [])

f3 :: Rule
f3 = (Head (Pred "married_" [Arg (Atom "homer"), Arg (Atom "marge") ]) [])

-- married(X,Y) :- married_(X,Y).
-- married(X,Y) :- married_(Y,X).
g1 :: Rule
g1 = (Head (Pred "married" [Arg (Var "X"), Arg (Var "Y") ]) [Predicate (Pred "_married" [ Arg (Var "X"), Arg (Var "Y" )])])

g2 :: Rule
g2 = (Head (Pred "married" [Arg (Var "X"), Arg (Var "Y" )]) [Predicate (Pred "_married" [Arg (Var "Y"), Arg (Var "X") ])])

-- eq(X, Y) :- X = Y.
e :: Rule
e = (Head (Pred "eq" [Arg (Var "X"),Arg ( Var "Y" )]) [Oper Eq (Arg (Var "X")) (Arg (Var "Y"))])

-- double(X, Y) :- Y is X * 2.
d :: Rule
d = (Head (Pred "double" [Arg (Var "X"), Arg (Var "Y" )]) [ Is (Arg (Var "Y")) (Oper Mult (Arg (Var "X")) (Arg (LitI 2))) ])


-- -- tree(leaf).
t1:: Rule
t1 = (Head (Pred "tree" [Arg (Atom "leaf")] ) [])

-- -- tree((node(3 , Leaf, Leaf) )).
-- (Head (Pred "node" [Arg (LitI 3), Arg (Atom "leaf"), Arg (Atom "leaf") ] ) [])


t2:: Rule
t2 = Head (Pred "tree" [Predicate (Pred "node" [ Arg (LitI 3), Arg (Atom "leaf"), Arg (Atom "leaf")])  ]  ) []


-- tree(node(4, leaf, node(3,leaf, node(4,leaf, leaf)))).
t3:: Rule
t3 = Head (Pred "tree" [Predicate (Pred "node" [ Arg (LitI 4), Arg (Atom "leaf"), Predicate (Pred "node" [ Arg (LitI 3), Arg (Atom "leaf"),  Predicate (Pred "node" [ Arg (LitI 4), Arg (Atom "leaf"), Arg (Atom "leaf")])])])]  ) []


-- isTree(leaf).
-- isTree(node(_, L, R)):- isTree(L),isTree(R).

i1 :: Rule
i1 = (Head (Pred "isTree" [Arg (Atom "leaf")] ) [])

i2 :: Rule
i2 = (Head (Pred "isTree" [Predicate (Pred "node" [ Arg (Var "_"), Arg (Var "L"), Arg (Var "R")])] ) [ Predicate (Pred "isTree" [ Oper And (Arg (Var "R")) (Arg (Var "L")) ])])




-- sumTree(leaf, 0 ).
-- sumTree(node(I, L, R), T ):- sumTree(L, N1), sumTree(R, N2), T is N1 + N2 + I.

s1:: Rule
s1 = (Head (Pred "sumTree" [Arg (Atom "leaf"), Arg (LitI 0)] ) [])


s2:: Rule
s2 = Head (Pred "sumTree" [Predicate (Pred "node" [ Arg (Var "I"), Arg (Var "L"), Arg (Var "R")]), Arg (Var "T") ])  [Oper And (Predicate (Pred "sumTree" [Arg (Var "L"), Arg (Var "N1")]) ) (Oper And  (Predicate (Pred "sumTree" [Arg (Var "R"), Arg(Var "N2")]) ) (Is (Arg(Var "T")) (Oper Add (Arg(Var "N1")) (Oper Add (Arg(Var "N2")) (Arg(Var "T"))) ))) ]



-- listLength([], 0).
-- listLength([_|T], Total):-  listLength(T, N) , Total is 1 + N.

l1:: Rule
l1 = (Head (Pred "listLength" [Arg (List []), Arg (LitI 0)] ) [])


l2:: Rule
l2 = (Head (Pred "listLength" [Arg (List ((Var "_"):[(Var "T")])), Arg (Var "Total") ]) [Oper And (Predicate (Pred "listLength" [Arg (Var "T"), Arg (Var "N")]) ) (Is (Arg (Var "Total")) (Oper Add  (Arg (LitI 1)) (Arg (Var "N")))) ])



typsdef :: TypeDef
typsdef = [treeType, nameType]


typspec :: TypeDic
typspec = [ d1, d2, d3, d4, d5, d6, d7 , d8 ]

prolog :: Prog
prolog = [v1, v2, v3, v4, v5, v6, v7, f1, f2, f3, g1, g2, e, d, t1, t2, t3, i1, t2, s1, s2, l1, l2]

domain :: Domain
domain (typsdef, typespec, prolog ) = Nothing

