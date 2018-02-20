%% --------------------------- %%
%% assignment 1 sample answers
%% --------------------------- %%
:- module(assignment1_answers,
  [candidate_number/1,
   q1/1,
   q2a/1,
   q2b/1,
   q3/1,
   q4a/1,
   q4b/1,
   q4c/1,
   q4d/1,
   q5_corner_move/0,
   q5_corner_move2/0,
   q6_spiral/1,
   q6_spiral_ks/2
  ]).

candidate_number(12345).

q1(ailp_start_position(p(X,Y))).
q1(ailp_start_position(P)).

q2a(new_pos(p(1,1), s, p(X,Y))).
q2a(new_pos(p(1,1), s, P)).

q2b(136).

q3([s,e,w,n]).

q4a(personal).
% This answer depends on your starting position (answer given for 12345 candidate number)
%q4a([p(2,3),p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(3,2),p(4,2),p(4,1),p(3,1),p(2,1),p(2,2),p(1,2),p(1,3),p(1,4)]).

q4b(personal).
% This answer depends on your starting position (answer given for 12345 candidate number)
%q4b([p(2,3),p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(3,2),p(4,2),p(4,1),p(3,1),p(2,1),p(2,2),p(1,2),p(1,1)]).

q4c(personal).
% This answer depends on your starting position (answer given for 12345 candidate number)
%q4c([p(2,3),p(2,4),p(1,4),p(1,3),p(1,2),p(2,2),p(3,2),p(3,3),p(3,4),p(4,4),p(4,3),p(4,2),p(4,1),p(3,1),p(2,1),p(1,1)]).

q4d(personal).
% This answer depends on your starting position (answer given for 12345 candidate number)
%q4d([p(2,3),p(2,4),p(1,4),p(1,3),p(1,2),p(1,1),p(2,1),p(2,2),p(3,2),p(3,3),p(3,4),p(4,4),p(4,3),p(4,2),p(4,1),p(3,1)]).

q5_corner_move :-
  assignment1:ailp_start_position(p(X,Y)),
  assignment1:ailp_show_move(p(X,Y),p(1,1)),
  assignment1:ailp_show_move(p(1,1),p(4,1)),
  assignment1:ailp_show_move(p(4,1),p(4,4)),
  assignment1:ailp_show_move(p(4,4),p(1,4)),
  assignment1:ailp_show_move(p(1,4),p(X,Y)).

q5_corner_move2 :-
  assignment1:ailp_start_position(p(X,Y)),
  assignment1:ailp_grid_size(N),
  assignment1:ailp_show_move(p(X,Y),p(1,1)),
  assignment1:ailp_show_move(p(1,1),p(N,1)),
  assignment1:ailp_show_move(p(N,1),p(N,N)),
  assignment1:ailp_show_move(p(N,N),p(1,N)),
  assignment1:ailp_show_move(p(1,N),p(X,Y)).

% Original q6 answer
q6_spiral(L) :-
  ailp_start_position(p(X,Y)),
  ailp_show_move(p(X,Y),p(1,1)),
  spiral_next(p(1,1),L).
spiral_next(P,L) :-
   spiral_next(P,L,[s,e,n,w]).
spiral_next(P,L,D) :-
  spiral_next(P,[P],Ps,D),
  reverse(Ps,L).
spiral_next(_,Ps,Ps,_) :-
  complete(Ps).
spiral_next(P,Ps,R,D) :-
  member(M,D),
  new_pos(P,M,P1),
  \+ memberchk(P1,Ps), !,
  ailp_show_move(P,P1),
  spiral_next(P1,[P1|Ps],R,[M|D]).
spiral_next(P,Ps,R,D) :-
  member(M,D),
  new_pos(P,M,P1),
  \+ memberchk(P1,Ps), !,
  ailp_show_move(P,P1),
  spiral_next(P1,[P1|Ps],R,D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% KS q6 answer
q6_spiral_ks([p(X, Y)|_], L) :-
  ailp_grid_size(S),
  ((X=1, Y=1);(X=1, Y=S);(X=S, Y=1);(X=S, Y=S)), %!,
  spiral_next_ks(p(X,Y), L).

spiral_next_ks(P,L) :-
   spiral_next_ks(P,L,[s,e,n,w]);
   spiral_next_ks(P,L,[e,s,w,n]).
spiral_next_ks(P,L,D) :-
  spiral_next_ks(P,[P],Ps,D),
  reverse(Ps,L).
spiral_next_ks(_,Ps,Ps,_) :-
  ailp_grid_size(X),
  XX is X * X,
  length(Ps, XX), !.
spiral_next_ks(P,Ps,R,D) :-
  member(M,D),
  new_pos(P,M,P1),
  \+ memberchk(P1,Ps), !,
  spiral_next_ks(P1,[P1|Ps],R,[M|D]).
spiral_next_ks(P,Ps,R,D) :-
  member(M,D),
  new_pos(P,M,P1),
  \+ memberchk(P1,Ps), !,
  spiral_next_ks(P1,[P1|Ps],R,D).
