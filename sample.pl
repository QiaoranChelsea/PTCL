data tree = node(int,tree,tree) | leaf.
type myList = list.

decl
female(atom).
age(atom, int).
married_(atom, atom).
# married(var, var).
# eq(var, var).
tree(tree).
isTree(tree).
sumTree(tree, int ).
listLength(myList, int).
end 

female(1).
# female(jacki).
# male(marge).

# age(mona, 6).
# age(jacki, 19).
# age(marge,20).

# doubleAge(A,T):-  age(Y,A) , T is A *2.

# married_(abe,mona).
# married_(clancy,jackie).
# married_(homer,marge).


# married(X,Y) :- married_(X,Y).
# married(X,Y) :- married_(Y,X).

# eq(X, Y) :- X = Y.
# double(X, Y) :- Y is X * 2.

# tree(leaf).
# tree(node(3 , Leaf, Leaf) ).
# tree(node(4, leaf, node(3,leaf, node(4,leaf, leaf)))).

# isTree(leaf).
# isTree(node(_, L, R)):- isTree(L),isTree(R).

# sumTree(leaf, 0 ).
# sumTree(node(I, L, R), T ):- sumTree(L, N1), sumTree(R, N2), T is N1 + N2 + I.

# listLength([], 0).
# listLength([_|T], Total):-  listLength(T, N) , Total is 1 + N.

