# data tree a = node(int,tree,tree) | leaf.
# data tree = node(int,tree,tree) | leaf.
# type myList = list.

decl 
female(atom).
end

female_(Vona).
female(jacki).
female(marge).

doubleAge(A,T):- age(A,Y),T is Y *2.
listLength([_|T], Total):-  listLength(T, N) , Total is 1 + N.