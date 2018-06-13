# Prolog_Type_Checking_Language (PTCL)
A DSL Project by Ghadeer Al Kubaish and Qiaoran Li.

The Prolog Type Checking language (PTCL) checks that predicates in a Prolog program are well-typed, where the type annotation describes the name of the predicate and the types of its arities. A Prolog program is well-typed when the predicates are satisfied with type annotation. By using Prolog Type Checking language, users can prevent type error and feel easier to debug the program with error and warning messages.


#  Instructions for running

You need to download SourcePos is from module Megaparsec. 
You should download it in your machine by typing  

$ cabal install megaparsec

To run the code

$ ghci RunPTCL.hs

To run the parser

$ runPTCL "sample.pl"

