
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

-- data tree _a = node(_a,tree _a ,tree _a ) | leaf
def1 :: DefinedType
def1 = ( DataT "tree" ["_a"] [("node", [(TVar "_a"), TDef "tree" ["_a"] , TDef "tree" ["_a"] ]),("leaf", []) ])

-- type myList = list
def2 :: DefinedType
def2 = ( TypeT "myList" TList )

-- data tree = node(string,tree ,tree ) | leaf
def3 :: DefinedType
def3 = ( DataT "tree" [][("node", [TString, TDef "tree" [] , TDef "tree" [] ]),("leaf", []) ])

-- data treeAB _a _b = node(_a, treeAB _a _b, treeAB _a _b) | leaf _b
def4 :: DefinedType
def4 = ( DataT "treeAB" ["_a", "_b"][("nodeAB", [(TVar "_a"), TDef "treeAB" ["_a", "_b"] , TDef "treeAB" ["_a", "_b"] ]),("leafAB", [TVar "_b"]) ])

-- | Declare predicates

-- decl predictes_name(type).

-- decl female(atom).
d1 :: Dec
d1 = PredD ("female", [TAtom])

-- decl female(string).
d2 :: Dec
d2 = PredD ("female", [TString])

-- decl age(atom,int).
d3 :: Dec
d3 = PredD ("age", [TAtom,TInt])

-- decl married_(atom, atom).
d4 :: Dec
d4 = PredD ("married_", [TAtom, TAtom])

-- decl married(atom, atom).
d5 :: Dec
d5 = PredD ("married", [TAtom, TAtom])

-- decl tree(tree _a ).
d6 :: Dec
d6 = PredD ("tree", [TDef "tree" ["_a"] ])

-- decl isTree(tree _a).
d7 :: Dec
d7 = PredD ("isTree", [TDef "tree" ["_a"] ])

-- decl sumTree(tree _a , int).
d8 :: Dec
d8 = PredD ("sumTree", [TDef "tree" ["_a"] , TInt])

-- decl listLength(myLi, int).
d9 :: Dec
d9 = PredD ("listLength", [TDef "myLi" [] , TInt]) -- error

-- decl eq(_a, _a).
d10 :: Dec
d10 = PredD ("eq", [TVar "_a",TVar "_a"])

-- decl eq6(int).
d11 :: Dec
d11 = PredD ("eq6", [TInt])

-- isEqual(int, atom).
d12 :: Dec
d12 = PredD ("isEqual", [TInt, TAtom])

-- decl eq2(_a, _b).
d13 :: Dec
d13 = PredD ("eq2", [TVar "_a",TVar "_b"])

-- decl comTwo(_a, _b).
d14 :: Dec
d14 = PredD ("comTwo", [TVar "_a",TVar "_b"])

-- decl formTree(_a, _b, treeAB _a _b).
d15 :: Dec
d15 = PredD ("formTree", [TVar "a",TVar "b", TDef "treeAB" ["a", "b"]])

-- decl doubleAge(atom, int).
d16 :: Dec
d16 = PredD ("doubleAge", [TAtom,TInt])

-- | Define predicates

-- male(mona).
e1 :: Rule
e1 = (Head ("male", [LitS "mona"]) [])

-- male(5).
e2 :: Rule
e2 = (Head ( "male", [ LitI 5]) [])

-- male(marge).
e3 :: Rule
e3 = (Head ( "male", [Atom "marge"]) [])

-- male(list).
e4 :: Rule
e4 = (Head ( "male", [List [(LitI 8)]]) [])

-- age(mona, 6).
e5 :: Rule
e5 = (Head ( "age", [ Atom "mona", LitI 6]) [])

-- age(jacki, 19, 19).
e6 :: Rule
e6 = (Head ( "age" ,[Atom "jacki", LitI 19, LitI 19 ]) [])

-- age(20,marge).
e7 :: Rule
e7 = (Head ( "age" ,[ LitI 20, Atom "marge"]) [])

-- doubleAge(A,T):-  age(Y,A) , T is A *2. -- error
e8 :: Rule
e8 = (Head ("doubleAge", [( Var "A"), (Var "T")]) [ And (Pred ( "age", [(Var "A"),(Var "Y")]) ) (Is ((Var "T")) (OperA Mult ( (Var "A")) ( (LitI 2)) )) ])
-- v7 = (Head ("doubleAge", [( Var "A"), (Var "T")]) [ And (Pred ( "age", [(Var "Y"),(Var "A")]) ) (Is ((Var "T")) (OperA Mult ( (Var "A")) ( (LitI 2)) )) ])

-- married_(abe,mona).
e9 :: Rule
e9 = (Head ( "married_", [ (Atom "abe"), (Atom "mona" )]) [])

-- married_(clancy,jackie).
e10 :: Rule
e10 = (Head ( "married_", [ (Atom "clancy"), (Atom "jackie") ]) [])

-- married_(homer,marge).
e11 :: Rule
e11 = (Head ( "married_", [ (Atom "homer"), (Atom "marge") ]) [])

-- married(X,Y) :- married_(X,Y).
-- married(X,Y) :- married_(Y,X).

e12 :: Rule
e12 = (Head ( "married", [ (Var "X"),  (Var "Y") ]) [Pred ( "married_", [  (Var "X"),  (Var "Y" )])])

e13 :: Rule
e13 = (Head ( "married", [ (Var "X"),  (Var "Y" )]) [Pred ( "married_", [ (Var "Y"),  (Var "X") ])])

-- eq(X, Y) :- X = Y.
e14 :: Rule
e14 = (Head ( "eq", [ (Var "X"), ( Var "Y" )]) [OperC Eq (Var "X") (Var "Y")])

-- ege6(X) :- eq(6,X).
e15 :: Rule
e15 = (Head ( "eq6", [( Var "X" )]) [(Pred ( "eq", [(LitI 6),(Var "X")]) )])

-- ege6(X) :- age(X,Y), eq(X,6).
e16 :: Rule
e16 = (Head ( "ege6", [( Var "X" )]) [ And (Pred ( "age", [(Var "X"),(Var "Y")]) ) (Pred ( "eq", [(Var "X"), (LitI 6)] ))])

-- ege6(X) :- age(X,Y), eq2(X,6).
e17 :: Rule
e17 = (Head ( "ege6", [( Var "X" )]) [ And (Pred ( "age", [(Var "X"),(Var "Y")]) ) (Pred ( "eq2", [(Var "X"), (LitI 6)] ))])

-- isEqual(X,Y) :- X = Y.
e18 :: Rule
e18 = (Head ( "isEqual", [( Var "X" ),( Var "Y" )]) [OperC Eq (Var "X") (Var "Y")])

-- comTwo(X,Y) :- X = 5 , age(X,Y).
e19 :: Rule
e19 = (Head ( "comTwo", [( Var "X" ),( Var "Y" )]) [And (OperC Eq (Var "X") (LitI 5)) (Pred ( "age", [(Var "X"),(Var "Y")]) )   ])

-- double(X, Y) :- X * 2 is Y.
e20 :: Rule
e20 = (Head ( "double", [ (Var "X"),  (Var "Y" )]) [ Is (OperA Mult  (Var "X")  (LitI 2)) (Var "Y") ])

-- tree(leaf).
e21:: Rule
e21 = (Head ( "tree", [(Atom "leaf")] ) [])

-- tree(node(leaf , leaf, leaf)).
e22:: Rule
e22 = Head ("tree", [ (Func ("node", [ (Atom "leaf"), (Atom "leaf"), (Atom "leaf")]))  ]  ) []

-- tree(node(4, leaf, node(3,leaf(4), node(4,leaf, leaf)))).
e23:: Rule
e23 = Head ("tree", [ (Func ("node", [ (LitI 4), (Atom "leaf"),  (Func ("node", [ (LitI 3), Func ("leaf", [(LitI 4)]) , (Func ("node", [ (LitI 4), (Atom "leaf"), (Atom "leaf")]))     ]))         ]))  ]  ) []

-- isTree(leaf).
e24 :: Rule
e24 = (Head ("isTree", [ (Atom "leaf")] ) [])

-- isTree(node(_, L, R)):- isTree(L),isTree(R).
e25 :: Rule
e25 = (Head ("isTree", [Func ( "node", [  (Var "_"),  (Var "L"),  (Var "R")])]) [ And (Pred ( "isTree", [(Var "R")])) (Pred ("isTree",[(Var "L")] ) ) ])

-- sumTree(0, leaf).
e26:: Rule
e26 = (Head ( "sumTree", [ (LitI 0), (Atom "leaf")] ) [])

-- sumTree(node(I, L, R), T ):- sumTree(L, N1), sumTree(R, N2), T is N1 + N2 + I.
-- sumTree(node(I, L, R), T ):- sumTree(L, N1), sumTree(R, L ), T is N1 + I.
e27:: Rule
e27 = Head ( "sumTree", [(Func ("node", [ (Var "I"), (Var "L"), (Var "R")])), (Var "T") ])  [ And (Pred ( "sumTree", [ (Var "L"),  (Var "N1")]) ) ( And  ( Pred ( "sumTree", [ (Var "R"), (Var "L")]) ) (Is (Var "T") (OperA Add (Var "N1") (Var "I") ))) ]

-- sumTree(node(I, L, R), T ):- sumTree(L, N1), sumTree(R, N2), T is N1 + R + I.
e28:: Rule
e28 = Head ( "sumTree", [(Func ("node", [ (Var "I"), (Var "L"), (Var "R")])), (Var "T") ])  [ And (Pred ( "sumTree", [ (Var "L"),  (Var "N1")]) ) ( And  ( Pred ( "sumTree", [ (Var "R"), (Var "N2")]) ) (Is (Var "T") (OperA Add (Var "N1") (OperA Add (Var "R") (Var "I")) ))) ]

-- formTree(X,Y, nodeAB(X,(leafAB(Y)),(leafAB(Y)))) :- X = 5 , Y = "S", X = Y. 
e29:: Rule
e29 = Head ( "formTree", [(Var "X"), (Var "Y") , Func ( "nodeAB", [  (Var "X"),  Func ( "leafAB", [(Var "Y")]) ,  Func ( "leafAB", [(Var "Y")]) ]) ]) [And (OperC Eq (Var "X") (LitI 5)) (And (OperC Eq (Var "Y") (LitS "S")) (Is (Var "Y") (Var "X"))) ]

-- formTree(X,Y, nodeAB(X,(leafAB(Y)),(leafAB(Y)))) :- X = 5 , Y = "S".
e30:: Rule
e30 = Head ( "formTree", [(Var "X"), (Var "Y") , Func ( "nodeAB", [  (Var "X"),  (Var "Y") ,  Func ( "leafAB", [(Var "Y")]) ]) ]) [And (OperC Eq (Var "X") (LitI 5)) (OperC Eq (Var "Y") (LitS "S"))]

-- listLength([], 0).
e31:: Rule
e31 = (Head ( "listLength", [ (List []),  (LitI 0)] ) [])

-- listLength([_|T], Total):-  listLength(T, N) , Total is 1 + N.
e32:: Rule
e32 = (Head ( "listLength", [ (List ((Var "_"):[(Var "T")])),  (Var "Total") ]) [ And (Pred ( "listLength", [ (Var "T"),  (Var "N")]) ) (Is ( (Var "Total")) (OperA Add   (LitI 1) (Var "N"))) ])


typsdef :: TypeDef
typsdef = [(def1, 1)
          ,(def2, 2)
          ,(def3, 3)
          ,(def4, 4)
          ]


typdec :: TypeDic
typdec = [ (d1, 5)
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
         , (d13, 17)
         , (d14, 18)
         , (d15, 19)
         , (d16, 20)

         ]

prolog :: Prog
prolog = [ 
           (e1, 21)
         , (e2, 22)
         , (e3, 23)
         , (e4, 24)
         , (e5, 25)
         , (e6, 26)
         , (e7, 27)
         , (e8, 28)
         , (e9, 29)
         , (e10, 30)
         , (e11, 31)
         , (e12, 32)
         , (e13, 33)
         , (e14, 34)
         , (e15, 35)
         , (e16, 36)
         , (e17, 37)
         , (e18, 38)
         , (e19, 39)
         , (e20, 40)
         , (e21, 41)
         , (e22, 42)
         , (e23, 43)
         , (e24,44)
         , (e25,45)
         , (e26,46)
         , (e27,47)
         , (e28,48)
         , (e29,49)
         , (e30,50)
         , (e31,51)
         , (e32, 20)
         ]


domain :: Domain
domain (typsdef, typdec, prolog ) = checker (typsdef, typdec, prolog )

-- domain :: Either ParseError Prolog
-- domain (Right (PL p )) = chcker p

v = putStrLn $ printReport (domain (typsdef, typdec, prolog )) typsdef prolog
-- --
-- --
-- k = putStrLn $ typeErrP (s3,44) typdec typsdef
