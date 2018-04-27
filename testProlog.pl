female(mona).
female(jacki).
female(marge).

age(mona, 6).
age(jacki, 19).
age(marge,20).

doubleAge(A,T):-  age(A,Y) , T is Y *2.

married_(abe,mona).
married_(clancy,jackie).
married_(homer,marge).

married(X,Y) :- married_(X,Y).
married(X,Y) :- married_(Y,X).

isEqual(X, Y) :- X = Y.
double(X, Y) :- Y is X * 2.

tree(leaf).
tree(node(4,leaf, leaf)).
tree(node(4, leaf, node(3,leaf, node(4,leaf, leaf)))).

isTree(leaf).
isTree(node(_, L, R)):- isTree(L),isTree(R).


sumTree(leaf, 0 ).
sumTree(node(I, L, R), T ):- sumTree(L, N1), sumTree(R, N2), T is N1 + N2 + I.

listLength([], 0).
listLength([_|T], Total):-  listLength(T, N) , Total is 1 + N.


%%%%%%%%%%%%%%%%%%%%%%%%%% invalid %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ageWrong(6,mona).
ageWrong('jacki4', 19).

femaleWrong(4).
femaleWrong('Ghadeer').
femaleWrong([5,6]).

doubleAgeWrong(A,T):-  age(A,_) , T is A *2.
doubleWrong(X, Y) :- X * 2 is Y.

marriedWrong(_,Y) :- married_('mona4',Y).
marriedWrong(_,Y) :- married_(Y,'mona4').

treeWrong(leaf(5)).
treeWrong(tree(4,5, leaf)).
treeWrong(4, leaf, tree(3,leaf, tree(leaf,leaf, leaf))).


