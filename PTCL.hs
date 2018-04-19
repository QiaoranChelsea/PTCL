module PTCL where

-- To DO: 1. How can we record the user defined type 


-- | The Domain of PTCL: Check the given Prolog file against type declaration,
--   and provide the report.
type Domain = (Prolog, TypeSpec) -> Report 

-- | The output of PTCL is True or False.
--   Should have more detail error message in the future
type Report = Bool

-- | A set of Prolog Predicate 
type Prolog = Undefined 

-- | A list of the type declaration 
type TypeSpec = [Decl]

-- 
-- * Basic Object of Type Declaration
-- 

-- | Type Declaration reoresented by:
--   The Name of the predicate associated with type of its arguments
type Decl = (Name, [Type])

-- | Name is the name of the predicate 
type Name = String 
-- (Shouldn't be the String here, but don't know what it should be )

-- | Basic Type in Prolog 
data Type = TAtom | TInt | TList



--
-- * Basic Object of Prolog Program 
--

-- | Predicate in Prolog, Name + Arguments
data Predicate = Pred Name [Argu] -- at least one element 

type Argu = Predicate 
          | Atom  
          | Expr

data Atom = Undefined

data Expr = Undefined

-- | Rules in Prolog, Head + Body 
data Rules = Head Predicate [Predicate]






