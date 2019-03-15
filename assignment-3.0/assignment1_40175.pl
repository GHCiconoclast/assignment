candidate_number(40175).
q1(ailp_start_position(Pos)).
q2a(new_pos(p(1,1),e,NewPos)).
q2b(136).
q3([s,e,w,n]).
q4a([p(4, 1), p(4, 2), p(4, 3), p(4, 4), p(3, 4), p(2, 4), p(1, 4), p(1, 3), p(2, 3), p(3, 3), p(3, 2), p(2, 2), p(1, 2), p(1, 1), p(2, 1), p(3, 1)]).
q4b([p(4, 1), p(4, 2), p(4, 3), p(4, 4), p(3, 4), p(2, 4), p(1, 4), p(1, 3), p(2, 3), p(3, 3), p(3, 2), p(2, 2), p(2, 1), p(3, 1)]).
q4c([p(4, 1), p(4, 2), p(4, 3), p(4, 4), p(3, 4), p(2, 4), p(1, 4), p(1, 3), p(2, 3), p(3, 3), p(3, 2), p(2, 2), p(1, 2), p(1, 1), p(2, 1), p(3, 1)]).

q5_corner_move:-
  ailp_show_move(p(1,1),p(1,4)),
  ailp_show_move(p(1,4),p(4,4)),
  ailp_show_move(p(4,4),p(4,1)).

q5_corner_move2:-
  ailp_grid_size(N),
  ailp_show_move(p(1,1),p(1,N)),
  ailp_show_move(p(1,N),p(N,N)),
  ailp_show_move(p(N,N),p(N,1)).

% Q6 doesn't work
q6_spiral_test([p(1,1)]).
q6_spiral_test([p(1,N)])    :-ailp_grid_size(N).
q6_spiral_test([p(N,1)])    :-ailp_grid_size(N).
q6_spiral_test([p(N,N)])    :-ailp_grid_size(N).
q6_spiral_test([P,PP,PPP|L]):-new_pos(PPP,Dir,PP),
                              new_pos(PP,Dir,P),
                              q6_spiral_test(L).
q6_spiral_test([P,PP|L]) :-m(M),
                           new_pos(PP,M,P),
                           q6_spiral_test(L).


%Add generator
% q6_spiral(L):-q6_spiral_test(L),
%               complete(L).

%Plagiarism

edge(p(1,K)):-ailp_grid_size(N),
              between(1,N,K).
edge(p(K,1)):-ailp_grid_size(N),
              between(1,N,K).
edge(p(N,K)):-ailp_grid_size(N),
              between(1,N,K).
edge(p(K,N)):-ailp_grid_size(N),
              between(1,N,K).

mprime(e).
mprime(s).
mprime(w).
mprime(n).

q6_spiral(L) :-
  edge(p(X,Y)),
  q6_spiral(p(X,Y),L),
  % q6_spiral_test(L),
  show_list_move(L).
% P: current position
% L: path taken by agentq6_spiral_test(L),
q6_spiral(P,L) :-
  q6_spiral(P,[P],Ps,0),
  reverse(Ps,L).
q6_spiral(_,Ps,Ps) :- complete(Ps).
q6_spiral(P,Ps,R,Deep) :-
  mprime(M),
  new_pos(P,M,P1),
  \+ memberchk(P1,Ps),
  inring(P1,Deep),
  q6_spiral(P1,[P1|Ps],R,Deep).
q6_spiral(P,Ps,R,Deep) :-
  mprime(M),
  new_pos(P,M,P1),
  \+ memberchk(P1,Ps),
  \+ inring(P1,Deep),
  ring_check(Ps,Deep),
  NewDepth is Deep + 1,
  q6_spiral(P1,[P1|Ps],R,NewDepth).

show_list_move([]).
show_list_move([P,PP|Ps]):-ailp_show_move(PP,P),
                           show_list_move([PP|Ps]).

ring_check(L,Deep):-ailp_grid_size(N),
                    Width is N - Deep * 2,
                    length(L,Length),
                    MinLength is 4 * Width - 4,
                    Length >= MinLength,
                    take(MinLength, RevL, RingL),
                    allInRing(RingL,Deep).

allInRing([],_Deep).
allInRing([P|Ps],Deep):-inring(P,Deep),
                        allInRing(Ps,Deep).

% Uses cut, can only be used for tests
inring(p(X,Y),Deep):-ailp_grid_size(N),
                     I is 1 + Deep,
                     J is N - Deep,
                     between(I,J,K),
                     inring(p(X,Y),I,J,K),!.
inring(p(I,K),I,_J,K).
inring(p(J,K),_I,J,K).
inring(p(K,I),I,_J,K).
inring(p(K,J),_I,J,K).
% Helper functions

head([H|_T],H).

% Taken from https://stackoverflow.com/questions/27151274/prolog-take-the-first-n-elements-of-a-list
take(N, _, Xs) :- N =< 0, !, N =:= 0, Xs = [].
take(_, [], []).
take(N, [X|Xs], [X|Ys]) :- M is N-1, take(M, Xs, Ys).
