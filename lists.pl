null([]).

head([X|_],X).

tail([_|X],X).

last([X],X).
last([_|Xs],Z):-last(Xs,Z).

init([_],[]).
init([X|Ys],[X|Zs]):-init(Ys,Zs).

nlength([],0).
nlength([_|Xs],N):-nlength(Xs,N0),N is N0+1.

sumList([],0).
sumList([X|Xs],N):-sumList(Xs,N0),N is N0+X.

nth(0,[Y|_],Y).
nth(X,[_|Ys],Z):-nth(X0,Ys,Z),X is X0+1.
