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

%Need mutually exclusive conditions
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
q6_spiral(L):-q6_spiral_test(L),
              complete(L).     

% Helper functions

head([H|T],H).
