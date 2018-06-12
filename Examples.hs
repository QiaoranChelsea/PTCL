
module Examples where

import Types
import Text.Megaparsec.Pos
import TypeChecker
import ErrorWarTypes
import Print
import Errors




--
-- * Examples
--

-- | Define types

-- data Tree = Node(Int,Tree,Tree) | Leaf
-- type MyList = List

treeType :: DefinedType
treeType = ( DataT "Tree" [] [("node", [TInt, TDef "Tree" [] , TDef "Tree" [] ]),("leaf", []) ])

nameType :: DefinedType
nameType = ( TypeT "MyList" TList )

treeType2 :: DefinedType
treeType2 = ( DataT "Tree" [][("node", [TString, TDef "Tree" [] , TDef "Tree" [] ]),("leaf", []) ])

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
d1 = PredD ("female", [TAtom])

d1_1 :: Dec
d1_1 = PredD ("female", [TString])

d2 :: Dec
d2 = PredD ("age", [TAtom,TInt])

d3 :: Dec
d3 = PredD ("married_", [TAtom, TAtom])

d4 :: Dec
d4 = PredD ("married", [TAtom, TAtom])

d5 :: Dec
d5 = PredD ("tree", [TDef "Tree" [] ])

d6 :: Dec
d6 = PredD ("isTree", [TDef "Tree" [] ])

d7 :: Dec
d7 = PredD ("sumTree", [TDef "Tree" [] , TInt])

d8 :: Dec
d8 = PredD ("listLength", [TDef "MyLi" [] , TInt]) -- error
-- d8 = PredD ("listLength", [TDef "MyList", TInt])


d9 :: Dec
d9 = PredD ("doubleAge", [TAtom,TInt])

d10 :: Dec
d10 = PredD ("eq", [TVar "a",TVar "a"])

d11 :: Dec
d11 = PredD ("eq6", [TInt])

d12 :: Dec
d12 = PredD ("isEqual", [TInt, TAtom])



--
-- %%%% valid program
--
-- female(mona).
-- female(jacki).
-- male(marge).
v1 :: Rule
v1 = (Head ("male", [LitS "mona"]) [])

v2 :: Rule
v2 = (Head ( "male", [ LitI 5]) [])

v3 :: Rule
v3 = (Head ( "male", [Atom "marge"]) [])

v3' :: Rule
v3' = (Head ( "male", [List [(LitI 8)]]) [])

-- age(mona, 6).
-- age(jacki, 19).
-- -- age(marge,20).
v4 :: Rule
v4 = (Head ( "age", [ Atom "mona", LitI 6]) [])

v5 :: Rule
v5 = (Head ( "age" ,[Atom "jacki", LitI 19, LitI 19 ]) [])

v6 :: Rule
v6 = (Head ( "age" ,[ LitI 20, Atom "marge"]) [])


-- doubleAge(A,T):-  age(Y,A) , T is A *2. -- error
v7 :: Rule
v7 = (Head ("doubleAge", [( Var "A"), (Var "T")]) [ And (Pred ( "age", [(Var "A"),(Var "Y")]) ) (Is ((Var "T")) (OperA Mult ( (Var "A")) ( (LitI 2)) )) ])
-- v7 = (Head ("doubleAge", [( Var "A"), (Var "T")]) [ And (Pred ( "age", [(Var "Y"),(Var "A")]) ) (Is ((Var "T")) (OperA Mult ( (Var "A")) ( (LitI 2)) )) ])


-- | Define predicates

-- married_(abe,mona).
-- married_(clancy,jackie).
-- married_(homer,marge).

f1 :: Rule
f1 = (Head ( "married_", [ (Atom "abe"), (Atom "mona" )]) [])

f2 :: Rule
f2 = (Head ( "married_", [ (Atom "clancy"), (Atom "jackie") ]) [])

f3 :: Rule
f3 = (Head ( "married_", [ (Atom "homer"), (Atom "marge") ]) [])

-- married(X,Y) :- married_(X,Y).
-- married(X,Y) :- married_(Y,X).
g1 :: Rule
g1 = (Head ( "married", [ (Var "X"),  (Var "Y") ]) [Pred ( "_married", [  (Var "X"),  (Var "Y" )])])

g2 :: Rule
g2 = (Head ( "married", [ (Var "X"),  (Var "Y" )]) [Pred ( "_married", [ (Var "Y"),  (Var "X") ])])

-- eq(X, Y) :- X = Y.
e :: Rule
e = (Head ( "eq", [ (Var "X"), ( Var "Y" )]) [OperC Eq (Var "X") (Var "Y")])

e' :: Rule
e' = (Head ( "eq6", [( Var "X" )]) [(Pred ( "eq", [(LitI 6),(Var "X")]) )])

e'' :: Rule
e'' = (Head ( "ege6", [( Var "X" )]) [ And (Pred ( "age", [(Var "X"),(Var "Y")]) ) (Pred ( "eq", [(Var "X"), (LitI 6)] ))])

e2 :: Rule
e2 = (Head ( "isEqual", [( Var "X" ),( Var "Y" )]) [OperC Eq (Var "X") (Var "Y")])


-- double(X, Y) :- Y is X * 2.
d :: Rule
d = (Head ( "double", [ (Var "X"),  (Var "Y" )]) [ Is (OperA Mult  (Var "X")  (LitI 2)) (Var "Y") ])

-- d = (Head ( "double", [ (Var "X"),  (Var "Y" )]) [ Is (Var "Y") (OperA Mult  (Var "X")  (LitI 2)) ])

-- tree(leaf).
t1:: Rule
t1 = (Head ( "tree", [(Atom "leaf")] ) [])

-- tree(node(3 , Leaf, Leaf)).
t2:: Rule
t2 = Head ("tree", [ (Func ("node", [ (LitI 3), (Atom "leaf"), (Atom "leaf")]))  ]  ) []

-- tree(node(4, leaf, node(3,leaf, node(4,leaf, leaf)))).
t3:: Rule
t3 = Head ("tree", [ (Func ("node", [ (LitI 4), (Atom "leaf"),  (Func ("node", [ (LitI 3), (Atom "leaf"), (Func ("node", [ (LitI 4), (Atom "leaf"), (Atom "leaf")]))     ]))         ]))  ]  ) []


-- isTree(leaf).
-- isTree(node(_, L, R)):- isTree(L),isTree(R).

i1 :: Rule
i1 = (Head ("isTree", [ (Atom "leaf")] ) [])

i2 :: Rule
i2 = (Head ("isTree", [Func ( "node", [  (Var "_"),  (Var "L"),  (Var "R")])]) [ And (Pred ( "isTree", [(Var "R")])) (Pred ("isTree",[(Var "L")] ) ) ])
--

-- sumTree(leaf, 0 ).
-- sumTree(node(I, L, R), T ):- sumTree(L, N1), sumTree(R, N2), T is N1 + N2 + I.

s1:: Rule
s1 = (Head ( "sumTree", [ (Atom "leaf"),  (LitI 0)] ) [])


s2:: Rule
s2 = Head ( "sumTree", [(Func ("node", [ (Var "I"), (Var "L"), (Var "R")])), (Var "T") ])  [ And (Pred ( "sumTree", [ (Var "L"),  (Var "N1")]) ) ( And  ( Pred ( "sumTree", [ (Var "R"), (Var "N2")]) ) (Is (Var "T") (OperA Add (Var "N1") (OperA Add (Var "N2") (Var "T")) ))) ]

-- listLength([], 0).
-- listLength([_|T], Total):-  listLength(T, N) , Total is 1 + N.

l1:: Rule
l1 = (Head ( "listLength", [ (List []),  (LitI 0)] ) [])

l2:: Rule
l2 = (Head ( "listLength", [ (List ((Var "_"):[(Var "T")])),  (Var "Total") ]) [ And (Pred ( "listLength", [ (Var "T"),  (Var "N")]) ) (Is ( (Var "Total")) (OperA Add   (LitI 1) (Var "N"))) ])


typsdef :: TypeDef
typsdef = [(treeType,  1)
          ,(treeType2, 2)
          ,( nameType,  3)]


typdec :: TypeDic
typdec = [ (d1_1, 4)
         , (d1, 5)
         , (d2, 6)
         , (d3, 7)
         , (d4, 8)
         , (d5, 9)
         , (d6, 10)
         , (d7, 11)
         , (d8, 12)
         , (d9, 13)
         , (d10, 14)
         , (d11, 15)
         , (d12, 16)
         
         
         ]

prolog :: Prog
prolog = [ (v1, 20)
         , (v2, 21)
         , (v3, 22)
         , (v3', 23)
         , (v4, 24)
         , (v5, 25)
         , (v6, 26)
         , (v7, 27)
         , (f1, 28)
         , (f2, 29)
         , (f3, 30)
         , (g1, 31)
         , (g2, 32)
         , (e, 33)
         , (d, 34)
         , (t1, 35)
         , (t2, 36)
         , (t3, 37)
         , (i1, 38)
         , (t2, 39)
         , (s1, 40)
         , (s2, 41)
         , (l1, 42)
         , (l2, 43)
         ,(e',44)
         ,(e'',45)
         ,(e2,46)
         
         ]


-- domain :: Domain
-- domain (typsdef, typdec, prolog ) = chcker (typsdef, typdec, prolog )

-- domain :: Either ParseError Prolog 
-- domain (Right (PL p )) = chcker p 

-- v = putStrLn $ printReport (domain (typsdef, typdec, prolog ))
-- --
-- --
-- k = putStrLn $ typeErrP (e',44) typdec typsdef
