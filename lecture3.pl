partition([],_,[],[]).
partition([Head|Tail],N,[Head|Littles],Bigs):-
  Head < N,
  partition(Tail,N,Littles,Bigs).
partition([Head|Tail],N,Littles,[Head|Bigs]):-
  Head >= N,
  partition(Tail,N,Littles,Bigs).

mySort([],[]).
mySort([Head|Tail],WholeSorted):-
  mySort(Tail,Sorted),
  insert(Head,Sorted,WholeSorted).

insert(X,[],[X]).
insert(X,[Head|Tail],[Head|Inserted]):-
  X > Head,
  insert(X,Tail,Inserted).
insert(X, [Head|Tail],[X,Head|Tail]):-
  X =< Head.
