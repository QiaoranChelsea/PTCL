data tree _a = node(_a,tree _a ,tree _a ) | leaf
type myList = list
data tree = node(string,tree ,tree ) | leaf
data treeAB _a _b = node(_a, treeAB _a _b, treeAB _a _b) | leaf _b


decl female(atom).
decl female(string).
decl age(atom,int).
decl married_(atom, atom).
decl married(atom, atom).
decl tree(tree _a ).
decl isTree(tree _a).
decl sumTree(tree _a , int).
decl listLength(myLi, int).
decl eq(_a, _a).
decl eq6(int).
isEqual(int, atom).
decl eq2(_a, _b).
decl comTwo(_a, _b).
decl formTree(_a, _b, treeAB _a _b).
decl doubleAge(atom, int).


male(mona).
male(5).
male(marge).
male(list).

age(mona, 6).
age(jacki, 19, 19).
age(20,marge).

doubleAge(A,T):-  age(Y,A) , T is A *2.

married_(abe,mona).
married_(clancy,jackie).
married_(homer,marge).

married(X,Y) :- married_(X,Y).
married(X,Y) :- married_(Y,X).

eq(X, Y) :- X = Y.
ege6(X) :- eq(6,X).
ege6(X) :- age(X,Y), eq(X,6).
ege6(X) :- age(X,Y), eq2(X,6).

isEqual(X,Y) :- X = Y.

comTwo(X,Y) :- X = 5 , age(X,Y).

double(X, Y) :- X * 2 is Y.

tree(leaf).
tree(node(leaf , leaf, leaf)).
tree(node(4, leaf, node(3,leaf(4), node(4,leaf, leaf)))).

isTree(leaf).
isTree(node(_, L, R)):- isTree(L),isTree(R).

sumTree(0, leaf).
sumTree(node(I, L, R), T ):- sumTree(L, N1), sumTree(R, L ), T is N1 + I.
sumTree(node(I, L, R), T ):- sumTree(L, N1), sumTree(R, N2), T is N1 + R + I.

formTree(X,Y, nodeAB(X,(leafAB(Y)),(leafAB(Y)))) :- X = 5 , Y = "S", X = Y.
formTree(X,Y, nodeAB(X,(leafAB(Y)),(leafAB(Y)))) :- X = 5 , Y = "S".

listLength([], 0).
listLength([_|T], Total):-  listLength(T, N) , Total is 1 + N.
