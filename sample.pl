sumTree(node(I, L, R), T ):- sumTree(L, N1), sumTree(R, N2), T is N1 + N2 + I.

# listLength([_|T], Total):-  listLength(T, N) , Total is 1 + N.
# data Tree = node(Int,Tree,Tree) | leaf.
# data Tree = node(Int,Tree,Tree) | leaf.
# type MyList = List.

# decl female(Atom).

# female_(Vona).
# female(jacki).
# female(marge).

# doubleAge(A,T):- age(A,Y),T is Y *2.