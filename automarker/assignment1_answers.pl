
%% --------------------------- %%
%% assignment 1 sample answers
%% --------------------------- %%

candidate_number(12345).

q1(ailp_start_position(p(X,Y))).

q2a(new_pos(p(1,1), s, p(X,Y))).

q2b(19).

q3([s,e,w,n]).

q4a(personal).

q4b(personal).

q4c(personal).

q4d(personal).

q5_corner_move :-
	ailp_start_position(X,Y),
	ailp_show_move(p(X,Y),p(1,1)),
	ailp_show_move(p(1,1),p(4,1)),
	ailp_show_move(p(4,1),p(4,4)),
	ailp_show_move(p(4,4),p(1,4)),
	ailp_show_move(p(1,4),p(X,Y)).
	
q5_corner_move2 :-
	ailp_start_position(X,Y),
	ailp_grid_size(N),
	ailp_show_move(p(X,Y),p(1,1)),
	ailp_show_move(p(1,1),p(N,1)),
	ailp_show_move(p(N,1),p(N,N)),
	ailp_show_move(p(N,N),p(1,N)),
	ailp_show_move(p(1,N),p(X,Y)).
	
	
q6_spiral(L) :-	
	ailp_start_position(X,Y),
	ailp_show_move(p(X,Y),p(1,1)),
	spiral_next(p(1,1),L).
	
	
spiral_next(P,L) :-
	 spiral_next(P,L,[s,e,n,w]).
	 
spiral_next(P,L,D) :- 
	spiral_next(P,[P],Ps,D),
	reverse(Ps,L).

spiral_next(_,Ps,Ps,_) :- complete(Ps).

spiral_next(P,Ps,R,D) :-
	member(M,D),
	new_pos(P,M,P1),
	\+ memberchk(P1,Ps),
	!,
	ailp_show_move(P,P1),
	spiral_next(P1,[P1|Ps],R,[M|D]).
	
spiral_next(P,Ps,R,D) :-
	member(M,D),
	new_pos(P,M,P1),
	\+ memberchk(P1,Ps),
	!,
	ailp_show_move(P,P1),
	spiral_next(P1,[P1|Ps],R,D).


	
	
