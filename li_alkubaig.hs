module PTCL where

-- | NOTE: 
--   1. ArguDic should store every declared type associated with name.

-- 
-- * The Domain of PTCL: Check the given Prolog file against type declaration,
--   and provide the report.

type Domain = (Prolog, TypeSpec) -> Maybe Error 

-- | The Error Message 
type Error = String


--
-- * Basic Object of Prolog Program 
--

-- | A set of Prolog Predicate 
type Prolog = [Rules] 

-- | Rules in Prolog, Head + Body 
data Rules = Head PredicateT Body

-- | Predicate in Prolog, Name + Arguments
data PredicateT = Pred PredName [ArguName] 

-- | Name of the Argument
type ArguName = String 

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

type VarName = String -- upper case 



-- | Argu Dictionary which stores every declared type associated with corresponding name.
--   The type infomation should in the top of file
--   * The ArguDic is different with the Decl.
--     Decl is the type annotation which declared before the rules
--     ArguDic is the type declaration about the value/argument 
type TypeDic = [(Type, [ArguName])]


--
-- * Basic Object of Type Declaration  
--


-- | A list of the type declaration 
type TypeSpec = [Decl]
 
-- | Type Declaration represented by:
--   The Name of the predicate associated with type of its arguments
type Decl = (PredName, [Type])

-- | Names
type PredName = String -- lower case 
 


type ConstructorName = String

-- | Types 

data Type = TAtom | TInt | TString| TList | TVar | D DefinedType

-- | Types for user defined type
data Constructor = ConstructorName [Type]
data DefinedType = TypeT TypeName Type | DataT TypeName Constructor
type TypeName = String

--
-- * Examples
--

-- type Atom = mona 
--           | jacki 
--           | marge 
--           | abe 
--           | clancy 
--           | homer 
-- type Var = X | Y 


typeDic :: TypeDic 
typeDic = [ (TAtom,[ "mona","jackie","marge","abe","clancy", "homer"])
          , (TVar,["X","Y"])
          , (TInt,["2"])]

-- female(Atom)

-- female(mona).
-- female(jacki).
-- female(marge).

v1 :: Rules 
v1 = (Head (Pred "female" ["mona"]) [])

v2 :: Rules 
v2 = (Head (Pred "female" ["jackie"]) [])

v3 :: Rules 
v3 = (Head (Pred "female" ["marge"]) [])




-- married_/2
-- married_(abe,mona).
-- married_(clancy,jackie).
-- married_(homer,marge).
f1 :: Rules 
f1 = (Head (Pred "married_" ["abe", "mona" ]) [])

f2 :: Rules 
f2 = (Head (Pred "married_" ["clancy", "jackie" ]) [])

f3 :: Rules 
f3 = (Head (Pred "married_" ["homer", "marge" ]) [])

-- married/2
-- married(X,Y) :- married_(X,Y).
-- married(X,Y) :- married_(Y,X).


g1 :: Rules 
g1 = (Head (Pred "married" ["X", "Y" ]) [Predicate (Pred "_married" [ "X", "Y" ])])

g2 :: Rules 
g2 = (Head (Pred "married" ["X", "Y" ]) [Predicate (Pred "_married" ["Y", "X" ])])

-- eq/2
-- eq(X, Y) :- X = Y.
eq :: Rules 
eq = (Head (Pred "eq" ["X", "Y" ]) [Oper Eq (Ref "X") (Ref "Y")])

-- double/2
-- double(X, Y) :- Y is X * 2.
double :: Rules 
double = (Head (Pred "double" ["X", "Y" ]) [Is (Ref "Y") (Oper Mult (Ref "X") (Lit 2))])

prolog :: [Rules]
prolog = [v1 , v2 , v3, f1, f2, f3, g1, g2, eq, double]


typspec :: TypeSpec 
typspec = [ ("married_",  [TAtom , TAtom , 
                             TAtom , TAtom , 
                             TAtom , TAtom  ])
          , ("married",  [TVar , TVar ])
          , ("_married",  [TVar , TVar ])
          , ("eq", [TVar, TVar ])
          , ("double" ,[TVar , TVar ])
          ]

domain :: Domain 
domain (prolog , typespec) = Nothing


