:- module(automarkrc,
  [switch/2,
   search_bounds/2,
   automarkrc/5,
   cn2sp/2,
   answerQ1/1,
   answerQ2a/1,
   answerQ2b/1,
   %q2b/1,
   answerQ3/1,
   answerQ4a/2,
   answerQ4b/2,
   answerQ4c/2,
   answerQ4d/2,
   answerQ5_corner_move/1,
   answerQ5_corner_move2/1,
   answerQ6/2
  ]).

switch(self_assessment,off).

search_bounds(100,10). % resolution depth, time in seconds

automarkrc(
   'assignment1_library.pl' % given Prolog code
  ,'ailp_assignment1.pl'    % submission
  ,'assignment1_answers.pl' % model answers
  ,'test1.pl'               % ground test queries
  ,[part(q1,1,[list(true->q1(Q1)->true)])
   ,part(q2a,1,[list(true->q2a(Q2a)->true)])
   ,part(q2b,1,[check(true->q2b(-Q2b)->[X2b]/(Q2b=X2b))])
   %,part(q2b,1,[check(true->q2b(-Q2b)->[_]/(answerQ2b(Q2b)))])
   ,part(q3,1,[check(true->q3(-Q3)->[X3]/(Q3=X3))])
   ,part(q4a,2,[check((candidate_number(CN),cn2sp(CN,SP))->q4a(-Q4a)->[_]/(answerQ4a(SP,X4a),X4a=Q4a))])
   ,part(q4b,2,[check((candidate_number(CN),cn2sp(CN,SP))->q4b(-Q4b)->[_]/(answerQ4b(SP,X4b),X4b=Q4b))])
   ,part(q4c,2,[check((candidate_number(CN),cn2sp(CN,SP))->q4c(-Q4c)->[_]/(answerQ4c(SP,X4c),X4c=Q4c))])
   ,part(q4d,2,[check((candidate_number(CN),cn2sp(CN,SP))->q4d(-Q4d)->[_]/(answerQ4d(SP,X4d),X4d=Q4d))])
   ,part(q5a,3,[check(((clause(q5_corner_move,B),am_writes(clause((q5_corner_move:-B))),nl)->q5_corner_move->true))])
   ,part(q5b,3,[check(((clause(q5_corner_move2,B),am_writes(clause((q5_corner_move2:-B))),nl)->q5_corner_move2->true))])
   ,part(q6,5,[list(true->q6_spiral(L)->true)])
   ,part(extra,2,[])
   ]
  ).

%cn2sp(12345,p(2,3)). % replace with program to generate starting position
% similar to start_position_personal in assignment1_library.pl
cn2sp(Z, p(X,Y)):-
  assignment1:ailp_grid_size(N),
  X is mod(Z,N) + 1,
  number_codes(Z,[A|[Y1|B]]),
  Y2 is Y1 - 48,
  Y is mod(Y2,N) + 1.

% Q1, Q2a, Q2b and Q3 answers -- the same for all students
answerQ1(ailp_start_position(p(X,Y))):-var(X),var(Y).
answerQ1(ailp_start_position(P)):-var(P).
answerQ2a(new_pos(p(1,1), s, p(X,Y))).
answerQ2a(new_pos(p(1,1), s, P)).
answerQ2b(109).
q2b(109).
answerQ3([s,e,w,n]).

% Q4a -- a look-up table with 16 possibilities
% answerQ4a(+start_position, -path).
answerQ4a(SP, Path) :-
  ( SP=p(1,1) -> Path = [p(1,1),p(1,2),p(1,3),p(1,4),p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(2,3),p(2,2),p(3,2),p(4,2),p(4,1),p(3,1),p(2,1)]
  ; SP=p(2,1) -> Path = [p(2,1),p(2,2),p(2,3),p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(3,2),p(4,2),p(4,1),p(3,1)]
  ; SP=p(3,1) -> Path = [p(3, 1), p(3, 2), p(3, 3), p(3, 4), p(4, 4), p(4, 3), p(4, 2), p(4, 1)]
  ; SP=p(4,1) -> Path = [p(4,1),p(4,2),p(4,3),p(4,4),p(3,4),p(2,4),p(1,4),p(1,3),p(2,3),p(3,3),p(3,2),p(2,2),p(1,2),p(1,1),p(2,1),p(3,1)]
  ; SP=p(1,2) -> Path = [p(1,2),p(1,3),p(1,4),p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(2,3),p(2,2),p(3,2),p(4,2),p(4,1),p(3,1),p(2,1),p(1,1)]
  ; SP=p(2,2) -> Path = [p(2,2),p(2,3),p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(3,2),p(4,2),p(4,1),p(3,1),p(2,1),p(1,1),p(1,2),p(1,3),p(1,4)]
  ; SP=p(3,2) -> Path = [p(3,2),p(3,3),p(3,4),p(4,4),p(4,3),p(4,2),p(4,1),p(3,1),p(2,1),p(2,2),p(2,3),p(2,4),p(1,4),p(1,3),p(1,2),p(1,1)]
  ; SP=p(4,2) -> Path = [p(4,2),p(4,3),p(4,4),p(3,4),p(2,4),p(1,4),p(1,3),p(2,3),p(3,3),p(3,2),p(2,2),p(1,2),p(1,1),p(2,1),p(3,1),p(4,1)]
  ; SP=p(1,3) -> Path = [p(1,3),p(1,4),p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(2,3),p(2,2),p(3,2),p(4,2),p(4,1),p(3,1),p(2,1),p(1,1),p(1,2)]
  ; SP=p(2,3) -> Path = [p(2,3),p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(3,2),p(4,2),p(4,1),p(3,1),p(2,1),p(2,2),p(1,2),p(1,3),p(1,4)]
  ; SP=p(3,3) -> Path = [p(3,3),p(3,4),p(4,4),p(4,3),p(4,2),p(3,2),p(2,2),p(2,3),p(2,4),p(1,4),p(1,3),p(1,2),p(1,1),p(2,1),p(3,1),p(4,1)]
  ; SP=p(4,3) -> Path = [p(4,3),p(4,4),p(3,4),p(2,4),p(1,4),p(1,3),p(2,3),p(3,3),p(3,2),p(4,2),p(4,1),p(3,1),p(2,1),p(2,2),p(1,2),p(1,1)]
  ; SP=p(1,4) -> Path = [p(1,4),p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(2,3),p(1,3),p(1,2),p(2,2),p(3,2),p(4,2),p(4,1),p(3,1),p(2,1),p(1,1)]
  ; SP=p(2,4) -> Path = [p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(2,3),p(1,3),p(1,4)]
  ; SP=p(3,4) -> Path = [p(3,4),p(4,4),p(4,3),p(3,3),p(2,3),p(2,4),p(1,4),p(1,3),p(1,2),p(2,2),p(3,2),p(4,2),p(4,1),p(3,1),p(2,1),p(1,1)]
  ; SP=p(4,4) -> Path = [p(4,4),p(3,4),p(2,4),p(1,4),p(1,3),p(2,3),p(3,3),p(4,3),p(4,2),p(3,2),p(2,2),p(1,2),p(1,1),p(2,1),p(3,1),p(4,1)]
  ).

% Q4b -- a look-up table with 16 possibilities
% answerQ4b(+start_position, -path).
answerQ4b(SP, Path) :-
  ( SP=p(1,1) -> Path = [p(1,1),p(1,2),p(1,3),p(1,4),p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(2,3),p(2,2),p(3,2),p(3,1),p(4,1),p(4,2)]
  ; SP=p(2,1) -> Path = [p(2,1),p(2,2),p(2,3),p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(3,2),p(3,1),p(4,1),p(4,2)]
  ; SP=p(3,1) -> Path = [p(3,1) ,p(3,2) ,p(3,3) ,p(3,4) ,p(2,4) ,p(1,4) ,p(1,3) ,p(2,3) ,p(2,2) ,p(1,2), p(1,1)]
  ; SP=p(4,1) -> Path = [p(4,1),p(4,2),p(4,3),p(4,4),p(3,4),p(2,4),p(1,4),p(1,3),p(2,3),p(3,3),p(3,2),p(2,2),p(2,1),p(3,1)]
  ; SP=p(1,2) -> Path = [p(1,2),p(1,3),p(1,4),p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(2,3),p(2,2),p(3,2),p(3,1),p(4,1),p(4,2)]
  ; SP=p(2,2) -> Path = [p(2,2), p(2,3), p(2,4), p(3,4), p(4,4), p(4,3), p(3,3), p(3,2), p(3,1), p(4,1), p(4,2)]
  ; SP=p(3,2) -> Path = [p(3,2),p(3,3),p(3,4),p(4,4),p(4,3),p(4,2),p(4,1),p(3,1),p(2,1),p(2,2),p(2,3),p(1,3),p(1,4),p(2,4)]
  ; SP=p(4,2) -> Path = [p(4,2),p(4,3),p(4,4),p(3,4),p(2,4),p(1,4),p(1,3),p(2,3),p(3,3),p(3,2),p(2,2),p(2,1),p(3,1),p(4,1)]
  ; SP=p(1,3) -> Path = [p(1,3),p(1,4),p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(2,3),p(2,2),p(3,2),p(3,1),p(4,1),p(4,2)]
  ; SP=p(2,3) -> Path = [p(2,3),p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(3,2),p(4,2),p(4,1),p(3,1),p(2,1),p(2,2),p(1,2),p(1,1)]
  ; SP=p(3,3) -> Path = [p(3,3),p(3,4),p(4,4),p(4,3),p(4,2),p(3,2),p(2,2),p(2,3),p(1,3),p(1,4),p(2,4)]
  ; SP=p(4,3) -> Path = [p(4,3),p(4,4),p(3,4),p(2,4),p(1,4),p(1,3),p(2,3),p(3,3),p(3,2),p(4,2),p(4,1),p(3,1),p(2,1),p(1,1),p(1,2),p(2,2)]
  ; SP=p(1,4) -> Path = [p(1,4),p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(2,3),p(1,3),p(1,2),p(2,2),p(3,2),p(3,1),p(4,1),p(4,2)]
  ; SP=p(2,4) -> Path = [p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(2,3),p(1,3),p(1,2),p(2,2),p(3,2),p(4,2),p(4,1),p(3,1),p(2,1),p(1,1)]
  ; SP=p(3,4) -> Path = [p(3,4),p(4,4),p(4,3),p(3,3),p(2,3),p(2,4),p(1,4),p(1,3),p(1,2),p(2,2),p(3,2),p(3,1),p(4,1),p(4,2)]
  ; SP=p(4,4) -> Path = [p(4,4),p(3,4),p(2,4),p(1,4),p(1,3),p(2,3),p(3,3),p(4,3),p(4,2),p(3,2),p(2,2),p(2,1),p(3,1),p(4,1)]
  ).

% Q4c -- a look-up table with 16 possibilities
% answerQ4c(+start_position, -path).
answerQ4c(SP, Path) :-
  ( SP=p(1,1) -> Path = [p(1,1),p(1,2),p(1,3),p(1,4),p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(2,3),p(2,2),p(3,2),p(4,2),p(4,1),p(3,1),p(2,1)]
  ; SP=p(2,1) -> Path = [p(2,1),p(2,2),p(2,3),p(3,3),p(3,2),p(3,1),p(4,1),p(4,2),p(4,3),p(4,4),p(3,4),p(2,4),p(1,4),p(1,3),p(1,2),p(1,1)]
  ; SP=p(3,1) -> Path = [p(3,1),p(3,2),p(3,3),p(2,3),p(2,2),p(2,1),p(1,1),p(1,2),p(1,3),p(1,4),p(2,4),p(3,4),p(4,4),p(4,3),p(4,2),p(4,1)]
  ; SP=p(4,1) -> Path = [p(4,1),p(4,2),p(4,3),p(4,4),p(3,4),p(2,4),p(1,4),p(1,3),p(2,3),p(3,3),p(3,2),p(2,2),p(1,2),p(1,1),p(2,1),p(3,1)]
  ; SP=p(1,2) -> Path = [p(1,2),p(1,3),p(1,4),p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(2,3),p(2,2),p(3,2),p(4,2),p(4,1),p(3,1),p(2,1),p(1,1)]
  ; SP=p(2,2) -> Path = [p(2,2),p(2,3),p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(3,2),p(4,2),p(4,1),p(3,1),p(2,1),p(1,1),p(1,2),p(1,3),p(1,4)]
  ; SP=p(3,2) -> Path = [p(3,2),p(3,3),p(3,4),p(4,4),p(4,3),p(4,2),p(4,1),p(3,1),p(2,1),p(2,2),p(2,3),p(2,4),p(1,4),p(1,3),p(1,2),p(1,1)]
  ; SP=p(4,2) -> Path = [p(4,2),p(4,3),p(4,4),p(3,4),p(2,4),p(1,4),p(1,3),p(2,3),p(3,3),p(3,2),p(2,2),p(1,2),p(1,1),p(2,1),p(3,1),p(4,1)]
  ; SP=p(1,3) -> Path = [p(1,3),p(1,4),p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(2,3),p(2,2),p(3,2),p(4,2),p(4,1),p(3,1),p(2,1),p(1,1),p(1,2)]
  ; SP=p(2,3) -> Path = [p(2,3),p(2,4),p(1,4),p(1,3),p(1,2),p(2,2),p(3,2),p(3,3),p(3,4),p(4,4),p(4,3),p(4,2),p(4,1),p(3,1),p(2,1),p(1,1)]
  ; SP=p(3,3) -> Path = [p(3,3),p(3,4),p(4,4),p(4,3),p(4,2),p(3,2),p(2,2),p(2,3),p(2,4),p(1,4),p(1,3),p(1,2),p(1,1),p(2,1),p(3,1),p(4,1)]
  ; SP=p(4,3) -> Path = [p(4,3),p(4,4),p(3,4),p(2,4),p(1,4),p(1,3),p(2,3),p(3,3),p(3,2),p(4,2),p(4,1),p(3,1),p(2,1),p(2,2),p(1,2),p(1,1)]
  ; SP=p(1,4) -> Path = [p(1,4),p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(2,3),p(1,3),p(1,2),p(2,2),p(3,2),p(4,2),p(4,1),p(3,1),p(2,1),p(1,1)]
  ; SP=p(2,4) -> Path = [p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(2,3),p(2,2),p(3,2),p(4,2),p(4,1),p(3,1),p(2,1),p(1,1),p(1,2),p(1,3),p(1,4)]
  ; SP=p(3,4) -> Path = [p(3,4),p(4,4),p(4,3),p(3,3),p(2,3),p(2,4),p(1,4),p(1,3),p(1,2),p(2,2),p(3,2),p(4,2),p(4,1),p(3,1),p(2,1),p(1,1)]
  ; SP=p(4,4) -> Path = [p(4,4),p(3,4),p(2,4),p(1,4),p(1,3),p(2,3),p(3,3),p(4,3),p(4,2),p(3,2),p(2,2),p(1,2),p(1,1),p(2,1),p(3,1),p(4,1)]
  ).

% Q4d -- a look-up table with 16 possibilities
% answerQ4d(+start_position, -path).
answerQ4d(SP, Path) :-
  ( SP=p(1,1) -> Path = [p(1,1),p(1,2),p(1,3),p(1,4),p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(2,3),p(2,2),p(2,1),p(3,1),p(3,2),p(4,2),p(4,1)]
  ; SP=p(2,1) -> Path = [p(2,1),p(2,2),p(3,2),p(3,1),p(4,1),p(4,2),p(4,3),p(4,4),p(3,4),p(3,3),p(2,3),p(2,4),p(1,4),p(1,3),p(1,2),p(1,1)]
  ; SP=p(3,1) -> Path = [p(3,1),p(3,2),p(2,2),p(2,1),p(1,1),p(1,2),p(1,3),p(1,4),p(2,4),p(2,3),p(3,3),p(3,4),p(4,4),p(4,3),p(4,2),p(4,1)]
  ; SP=p(4,1) -> Path = [p(4,1),p(4,2),p(4,3),p(4,4),p(3,4),p(2,4),p(1,4),p(1,3),p(2,3),p(3,3),p(3,2),p(3,1),p(2,1),p(2,2),p(1,2),p(1,1)]
  ; SP=p(1,2) -> Path = [p(1,2),p(1,3),p(1,4),p(2,4),p(3,4),p(4,4),p(4,3),p(4,2),p(4,1),p(3,1),p(3,2),p(3,3),p(2,3),p(2,2),p(2,1),p(1,1)]
  ; SP=p(2,2) -> Path = [p(2,2),p(2,3),p(2,4),p(1,4),p(1,3),p(1,2),p(1,1),p(2,1),p(3,1),p(3,2),p(3,3),p(3,4),p(4,4),p(4,3),p(4,2),p(4,1)]
  ; SP=p(3,2) -> Path = [p(3,2),p(3,3),p(3,4),p(4,4),p(4,3),p(4,2),p(4,1),p(3,1),p(2,1),p(1,1),p(1,2),p(1,3),p(1,4),p(2,4),p(2,3),p(2,2)]
  ; SP=p(4,2) -> Path = [p(4,2),p(4,3),p(4,4),p(3,4),p(2,4),p(1,4),p(1,3),p(1,2),p(1,1),p(2,1),p(2,2),p(2,3),p(3,3),p(3,2),p(3,1),p(4,1)]
  ; SP=p(1,3) -> Path = [p(1,3),p(1,4),p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(2,3),p(2,2),p(1,2),p(1,1),p(2,1),p(3,1),p(3,2),p(4,2),p(4,1)]
  ; SP=p(2,3) -> Path = [p(2,3),p(2,4),p(1,4),p(1,3),p(1,2),p(1,1),p(2,1),p(2,2),p(3,2),p(3,3),p(3,4),p(4,4),p(4,3),p(4,2),p(4,1),p(3,1)]
  ; SP=p(3,3) -> Path = [p(3,3),p(3,4),p(4,4),p(4,3),p(4,2),p(4,1),p(3,1),p(3,2),p(2,2),p(2,3),p(2,4),p(1,4),p(1,3),p(1,2),p(1,1),p(2,1)]
  ; SP=p(4,3) -> Path = [p(4,3),p(4,4),p(3,4),p(2,4),p(1,4),p(1,3),p(2,3),p(3,3),p(3,2),p(4,2),p(4,1),p(3,1),p(2,1),p(1,1),p(1,2),p(2,2)]
  ; SP=p(1,4) -> Path = [p(1,4),p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(2,3),p(1,3),p(1,2),p(1,1),p(2,1),p(2,2),p(3,2),p(4,2),p(4,1),p(3,1)]
  ; SP=p(2,4) -> Path = [p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(3,2),p(4,2),p(4,1),p(3,1),p(2,1),p(1,1),p(1,2),p(2,2),p(2,3),p(1,3),p(1,4)]
  ; SP=p(3,4) -> Path = [p(3,4),p(4,4),p(4,3),p(3,3),p(2,3),p(2,4),p(1,4),p(1,3),p(1,2),p(1,1),p(2,1),p(2,2),p(3,2),p(4,2),p(4,1),p(3,1)]
  ; SP=p(4,4) -> Path = [p(4,4),p(3,4),p(2,4),p(1,4),p(1,3),p(2,3),p(3,3),p(4,3),p(4,2),p(4,1),p(3,1),p(3,2),p(2,2),p(1,2),p(1,1),p(2,1)]
  ).

% Q5 -- visit all the corners
% answerQ5_corner_move([answer predicates]).
% answerQ5_corner_move2([answer predicates]).
answerQ5_corner_move([
  ailp_start_position(p(X,Y)),
  ailp_show_move(p(X,Y),p(1,1)),
  ailp_show_move(p(1,1),p(4,1)),
  ailp_show_move(p(4,1),p(4,4)),
  ailp_show_move(p(4,4),p(1,4)),
  ailp_show_move(p(1,4),p(X,Y))
]).
answerQ5_corner_move2([
  ailp_start_position(p(A1,A2)),
  ailp_show_move(p(A1,Y),p(1,1)),
  ailp_show_move(p(1,1),p(4,1)),
  ailp_show_move(p(4,1),p(4,4)),
  ailp_show_move(p(4,4),p(1,4)),
  ailp_show_move(p(1,4),p(X,Y))
]).

% Q6 -- all possible spirales
% answerQ6(+start_position, -path).
answerQ6(p(X, Y), L) :-
  ailp_grid_size(S),
  ((X=1, Y=1);(X=1, Y=S);(X=S, Y=1);(X=S, Y=S)), %!,
  spiral_next(p(X,Y), L).

spiral_next(P,L) :-
  spiral_next(P,L,[s,e,n,w]);
  spiral_next(P,L,[e,s,w,n]).

spiral_next(P,L,D) :-
  spiral_next(P,[P],Ps,D),
  reverse(Ps,L).

spiral_next(_,Ps,Ps,_) :-
  ailp_grid_size(X),
  XX is X * X,
  length(Ps, XX), !.

spiral_next(P,Ps,R,D) :-
  member(M,D),
  new_pos(P,M,P1),
  \+ memberchk(P1,Ps), !,
  spiral_next(P1,[P1|Ps],R,[M|D]).

spiral_next(P,Ps,R,D) :-
  member(M,D),
  new_pos(P,M,P1),
  \+ memberchk(P1,Ps), !,
  spiral_next(P1,[P1|Ps],R,D).
