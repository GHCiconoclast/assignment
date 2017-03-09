:- module(automark,
          [gentest/0,
           test/0,
           candidate_number/1 % reexport candidate_number form user submission
          ]).

:- dynamic am_student/1.

:- prolog_load_context(directory, Sys),
  asserta(user:file_search_path(local_library, Sys)),
  atomic_list_concat([Sys, "..", ".."], "/", Root),
  asserta(user:file_search_path(local_root, Root)).

:- use_module(local_library('assignment1_automarkrc.pl')).

gentest :-
  automarkrc(LabFile,_,AnswersFile,TestFile,Parts),
  use_module(local_library(LabFile)),
  assignment1:set_switch(headless, true),
  use_module(local_library(AnswersFile)),
  absolute_file_name(local_root(TestFile), TF),
  tell(TF),
  gentest_parts(Parts).

gentest_parts([]) :-
  told.
gentest_parts([part(_,_,Queries)|Parts]) :-
  gentest_queries(Queries),
  gentest_parts(Parts).

gentest_queries([]).
gentest_queries([Q|Qs]) :-
  gentest_query(Q),
  gentest_queries(Qs).

gentest_query(count(Pre->Q->_)) :-
  call(Pre),
  strip_call(Q),
  am_portray_clause(testcase(Q)),
  fail.
gentest_query(check(Pre->Q->_)) :-
  call(Pre),
  strip_call(Q),
  am_portray_clause(testcase(Q)),
  fail.
gentest_query(list(_)) :-
  fail.
gentest_query(once(_)) :-
  fail.
gentest_query(_).

test :-
  test_prelims1(CwFile,Parts),
  ( catch(load_files(CwFile,[]),_,fail) -> test_prelims2(Parts),
                                     test_parts2(Parts)
  ; otherwise -> am_error(nofile(CwFile)),told,halt
  ).

qtest :-
  test_prelims1(CwFile,Parts),
  ( catch(load_files(CwFile,[]),_,fail) -> test_prelims2(Parts),
                                     qtest_parts2(Parts)
  ; otherwise -> am_error(nofile(CwFile)),told,halt
  ).

test_prelims1(CwFile,Parts) :-
  automarkrc(LabFile,CwFile,_,TestFile,Parts),
  % we assume we're in the student's sub-directory
  ( LabFile = '' -> true
  ; use_module(local_library(LabFile)), assignment1:set_switch(headless, true)
  ),
  ( TestFile = '' -> true
  ; consult(local_root(TestFile))
  ),
  tell('comments.txt'),
  catch(consult('commentsPF.pl'),_,true).

test_prelims2(Parts) :-
  am_writes(['Marks breakdown',nl]),
  test_parts1(Parts),
  ( clause(commentsPF(_,_),_) -> get_commentsPF ; true ),
  ( switch(self_assessment,on) -> am_writes(['Student self-assessment',nl]),
                                  self_assessment(Parts,0)
  ; true
  ),
  am_writes([nl,'=-=-=-=-=-=-=-= Output of automark follows =-=-=-=-=-=-=-=',nl,nl]).

test_parts1([]) :- am_nl.
test_parts1([part(Label,Weight,_)|Parts]) :-
  am_writes([Label,': XX/',Weight,' ',nl]),
  test_parts1(Parts).

self_assessment([],Total) :-
  ( Total > 0 -> am_writes(['Student mark: ',Total,nl])
  ; otherwise -> am_write_comments('Missing self-assessment',-1)
  ).
  %( clause(am_student(S),_) -> Output='student_marks.txt'
  %; otherwise -> Output=user_output,S='Student mark'
  %),
  %telling(CurrentStream),told,tell(Output),am_writes([S,' ',Total,nl]),told,append(CurrentStream).
self_assessment([part(Label,_,_)|Parts],RunningTotal) :-
  ( clause(student_mark(_,_,_),_),student_mark(Label,Mark/Weight,Comment) -> am_writes([Label,': ',Mark/Weight,' - ',Comment,nl])
  ; otherwise -> am_error(self_assessment(Label)),Mark=0
  ),
  NewRunningTotal is RunningTotal+Mark,
  self_assessment(Parts,NewRunningTotal).

get_commentsPF  :-
  am_writes(['Marker comments',nl]),
  commentsPF(C,M), am_write_comments(C,M), fail.
get_commentsPF  :-
  am_nl.

test_parts2([]) :-
  told.
test_parts2([part(Label,_,Queries)|Parts]) :-
  ( Label=extra -> true
  ; Queries=[]  -> true
  ; otherwise -> am_writes([nl,'*** Queries for ',Label,' ***',nl,nl]),
                 test_queries(Queries),
                 test_parts2(Parts)
  ).

qtest_parts2([]) :-
  told.
qtest_parts2([part(Label,_,Queries)|Parts]) :-
  ( Label=extra -> true
  ; Queries=[]  -> true
  ; otherwise -> am_writes([nl,'*** Queries for ',Label,' ***',nl,nl]),
                 qtest_queries(Queries),
                 qtest_parts2(Parts)
  ).

test_queries([]).
test_queries([Q|Qs]) :-
  %( \+ \+ time(catch(test_query(Q),E,fail)) -> true
  %; Q =.. [_,(_->Q1->_)] -> am_error(exception(Q1,E))
  %),test_queries(Qs).
  \+ \+ time(catch(test_query(Q),E,(Q =.. [_,(_->Q1->_)],am_error(exception(Q1,E))))),
  test_queries(Qs).

qtest_queries([]).
qtest_queries([Q|Qs]) :-
  qtest_query(Q),
  qtest_queries(Qs).

qtest_query(Q0) :-
  Q0 =.. [Kind,(Pre->Q1->Post)],
  telling(Old),tell(user),
  am_portray_clause(Q0),
  write('CALL? [y/n] '),
  told,tell(Old),
  read(Answer),
  ( Answer = y -> Q=Q0
  ; otherwise -> Q =.. [skip,(Pre->Q1->Post)]
  ),
  \+ \+ time(catch(test_query(Q),E,am_error(exception(Q1,E)))).

test_query(count(Pre->Q->Post)) :-
  pre_query(Pre),
  ( bad_query(Q) -> true
  ; otherwise -> count_query(Q->Post)
  ),fail.
test_query(check(Pre->Q->Post)) :-
  pre_query(Pre),
  ( bad_query(Q) -> true
  ; otherwise -> check_query(Q->Post)
  ),fail.
test_query(checkm(Pre->Q->Post)) :-
  pre_query(Pre),
  ( bad_query(Q) -> true
  ; otherwise -> checkm_query(Q->Post)
  ),fail.
test_query(compare(Pre->Q->Post)) :-
  pre_query(Pre),
  ( bad_query(Q) -> true
  ; otherwise -> compare_query(Q->Post)
  ),fail.
test_query(list(Pre->Q->_)) :-
  pre_query(Pre),
  ( bad_query(Q) -> true
  ; otherwise -> list_query(Q)
  ),fail.
test_query(once(Pre->Q->_)) :-
  pre_query(Pre),
  ( bad_query(Q) -> true
  ; otherwise -> once_query(Q)
  ),am_nl,fail.
test_query(skip(Pre->Q->_)) :-
  pre_query(Pre),
  am_writes(skip(Q)),fail.
test_query(_) :- am_nl.

pre_query(Pre) :-
  %( \+ clause(Pre,_) -> am_error(undefined(Pre)),fail
  %; otherwise -> catch(call(Pre),E,(am_error(exception(Pre,E)),fail) )
  %).
  catch(call(Pre),E,(am_error(exception(Pre,E)),fail)).
%pre_query(Pre) :-
%am_error(failure(Pre)),!,fail.

count_query(Q->Vars0/Post) :-
  am_writes(count(Q)),
  true_positives(Q->Vars0/Post,TP),
  false_positives(Q->Vars0/Post,LFP,FP),
  false_negatives(Q->Vars0/Post,LFN,FN),
  am_writes([tab(5),'     ',TP,'   ',FP,nl]),
  am_writes([tab(5),'     ',FN,nl]),
  ( Pos is TP+FN,Pos>0 -> P is TP/Pos ; P=0 ),
  ( PPos is TP+FP,PPos>0 -> R is TP/PPos ; R=0 ),
  ( PR is P+R,PR>0 -> F is (2*P*R)/PR ; F=0 ),
  am_writes([tab(5),'Score: ',F,' (Precision: ',P,'; Recall: ',R,')',nl]),
  ( F=1 -> am_writes([same_answers,nl])
  ; otherwise ->  ( FP>0 -> am_writes([ am_found|LFP]),am_nl ; true ),
  ( FN>0 -> am_writes([you_found|LFN]) ; true )
  ),am_nl,am_nl.

true_positives(Q->Vars0/Post,TP) :-
  copy_term(Q,Q0),
  am_count(Q,Q0^Vars0^(strip_call(Q), (mtestcase(Q0,Vars0),Post)),TP).

false_negatives(Q->Vars0/Post,L,FN) :-
  copy_term(Q,Q0),
  %am_count(Q,Q0^Vars0^(strip_call(Q),\+ (mtestcase(Q0,Vars0),Post)),FN).
  am_count(answers(Vars),Q^Q0^Vars0^(strip_call_getAnswers(Q,Vars),\+ (mtestcase(Q0,Vars0),Post)),L,FN).

false_positives(Q->Vars0/Post,L,FP) :-
  copy_term(Q,Q0),
  %am_count(Q,Q0^Vars0^(mtestcase(Q0,Vars0),\+ (strip_call(Q),Post)),FP).
  am_count(answers(Vars0),Q^Q0^Vars0^(mtestcase(Q0,Vars0),\+ (strip_call(Q),Post)),L,FP).

am_count(X,Q,N) :-
  am_count(X,Q,_,N).

am_count(X,Q,L,N) :-
  setof(X,Q,L),!,
  length(L,N).
am_count(_X,_Q,[],0).

checkm_query(Q->Vars0/Post/Message) :-
  am_writes(check(Q)),
  copy_term(Q,Q0),
  strip_call_getAnswers(Q,Vars),
  %!, % temporarily only ask for one answer
  mtestcase(Q0,Vars0),
  Post,
  am_writes([Message,nl]),
  ( Message=same_answers -> true
  ; otherwise -> am_writes([am_found,answers(Vars0),you_found,answers(Vars),nl,nl])
  ).

check_query(Q->Vars0/Post) :-
  am_writes(check(Q)),
  copy_term(Q,Q0),
  strip_call_getAnswers(Q,Vars),
  %!, % temporarily only ask for one answer
  mtestcase(Q0,Vars0),
  ( Post ->      am_writes([tab(5),answers(Vars),'ALL ANSWERS CORRECT.',nl,nl])
  ; otherwise -> am_error(incorrect(Vars,Vars0))
  ).

compare_query(Q->Vars0/Post) :-
  am_writes(compare(Q)),
  copy_term(Q,Q0),
  strip_call_getAnswers(Q,Vars),
  %!, % temporarily only ask for one answer
  mtestcase(Q0,Vars0),
  ( Post      -> am_writes([tab(5),answers(Vars),same_answers,nl,nl])
  ; otherwise -> am_writes([am_found,answers(Vars0),you_found,answers(Vars),nl,nl])
  ).

bad_query(Q) :-
  ( strip(Q,Q1),\+clause(Q1,_) -> am_error(undefined(Q))
  ; \+strip_prove_d(Q) -> am_error(failure(Q)) % temporarily disabled
  ; \+strip_call_modeCheck(Q) -> am_error(variable(Q)) % temporarily disabled
  ).

list_query(Q) :-
  am_writes(list(Q)),
  strip_call(Q),
  am_writes([tab(5),clause(Q)]).

once_query(Q) :-
  am_writes(once(Q)),
  strip_call(Q),!,
  am_writes([tab(5),clause(Q)]).

strip_prove_d(Q) :-
  strip(Q,Q1),
  am_prove_d(Q1).

strip_call_modeCheck(Q) :-
  strip(Q,Q1),
  strip_call(Q1),
  mode_check(Q,Q1).

% Currently succeeds when depth bounds exceeded -- need to fix this
am_prove_d(Q) :-
  automarkrc(_,F,_,_,_),
  search_bounds(D,T1),
  get_time(T0),T is T0+T1,
  am_prove_d(Q,D,T,F).

am_prove_d(true,_,_,_) :- !.
am_prove_d((A,B),D,T,F) :- !,
  am_prove_d(A,D,T,F),
  am_prove_d(B,D,T,F).
am_prove_d(A,D,T,F) :-
  predicate_property(A,file(F0)),
  atom_concat(_,F,F0),!,
  get_time(T1),
  search_bounds(DD,TT),
  ( D =< 0 -> am_error(depth(A,DD)),fail
  ; T1 > T -> am_error(time(A,TT)),fail
  ; otherwise -> D1 is D-1,clause(A,B),am_prove_d(B,D1,T,F)
  ).
am_prove_d(A,_,_,_) :-
  call(A).

mtestcase(Q,OutArgs) :-
  strip(Q,_,OutArgs),
  testcase(Q).

strip_call(Q) :-
  strip_call_getAnswers(Q,_).

strip_call_getAnswers(Q,OutArgs) :-
  strip(Q,Q1,OutArgs),
  call(Q1).

strip(Q,Q1) :-
  strip(Q,Q1,_).

strip(Q,Q1,OutArgs) :-
  Q =.. [P|Args],
  strip_l(Args,Args1,OutArgs),
  Q1 =.. [P|Args1].

strip_l([],[],[]).
strip_l([A|As],[A|As1],As2) :-
  var(A),!,
  strip_l(As,As1,As2).
strip_l([A|As],[A1|As1],As2) :-
  functor(A,F,_),
  ( F==(+) -> A=(+A1),As2=As3
  ; F==(-) -> A=(-A1),As2=[A1|As3]
  ; otherwise -> A1=A,As2=As3
  ),strip_l(As,As1,As3).

mode_check(Q,Q1) :-
  Q =.. [P|Args],
  Q1 =.. [P|Args1],
  mode_check_l(Args,Args1).

mode_check_l([],[]).
mode_check_l([A|As],[A|As1]) :-
  var(A),!,
  mode_check_l(As,As1).
mode_check_l([A|As],[A1|As1]) :-
  ( functor(A,F,_),F == - -> ground(A1)
  ; otherwise -> true
  ),mode_check_l(As,As1).

am_error(Error) :-
  am_writes(['AUTOMARK ERROR -- ']),
  am_error(Error,Message,Post),
  am_writes(Message),
  call(Post).

am_error(self_assessment(L),['no self-assessment for question ',L,'.'],am_nl).
am_error(nofile(F),['file does not exist: ',F,'.',nl],am_nl).
am_error(undefined(P),['no clause for query: ',clause(P),'.'],am_nl).
am_error(exception(Q),['query raises exception: ',clause(Q)],am_nl).
am_error(exception(Q,E),['query raises exception: ',clause(Q)],(append('comments.txt'),print_message(error,E),told)).
am_error(failure(Q),['query fails: ',clause(Q)],am_nl).
am_error(depth(Q,D),['resolution depth bound (',D,') reached: ',clause(Q)],am_nl).
am_error(time(Q,T),['time bound (',T,') reached: ',clause(Q)],am_nl).
am_error(variable(Q),['query returns variable: ',clause(Q)],am_nl).
am_error(incorrect(A,B),['incorrect answer:   ',answers(A),'               -- too different from: ',answers(B)],am_nl).
%am_error(nontermination(Q),['depth or time bound reached: ',clause(Q)],am_nl). % obsolete

am_write_comments(C,M) :-
  am_writes(C),
  ( M >  1 -> am_writes([' -- you get ',M,' extra marks for that.',nl])
  ; M =  1 -> am_writes([' -- you get 1 extra mark for that.',nl])
  ; M =  0 -> am_writes(['.',nl])
  ; M = -1 -> am_writes([' -- you lose 1 mark for that.',nl])
  ; M < -1 -> M1 is -M, am_writes([' -- you lose ',M1,' marks for that.',nl])
  ).

am_writes(X) :-
  append('comments.txt'),
  ( X=[] -> true
  ; X=nl -> nl
  ; X=clause(C) -> strip(C,C1),am_portray_clause(C1)
  ; X=answers(A) -> strip(A,A1),am_portray_clause(A1)
  ; X=[H|T] -> am_writes(H),am_writes(T)
  ; X=tab(N) -> tab(N)
  ; X=same_answers -> write('SAME ANSWERS FOUND AS AUTOMARK.')
  ; X= am_found -> write('*** AUTOMARK FOUND: ')
  ; X=you_found -> write('******** YOU FOUND: ')
  ; X=skip(Q) -> am_writes(['SKIPPING query ?-',clause(Q)])
  ; X=compare(Q) -> am_writes(['COMPARING answers to ?-',clause(Q)])
  ; X=check(Q) -> am_writes(['CHECKING answers to ?-',clause(Q)])
  ; X=list(Q) -> am_writes(['LISTING answers to ?-',clause(Q)])
  ; X=once(Q) -> am_writes(['FIRST answer to ?-',clause(Q)])
  ; X=count(Q) -> am_writes(['SCORING answers to ?-',clause(Q)])
  ; otherwise -> write(X)
  ),told.

am_nl :- am_writes([nl]).

am_portray_clause(A) :-
  \+ \+ (numbervars(A, 0, _), portray_clause(A)).

%:- ( test -> halt ; am_writes(['Automatic testing failed.',nl]),told,halt ).
