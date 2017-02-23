
1. download student files from FEN.

2. Put these files in the ASGNM directory: 
assignment1_answers.pl
assignment1_library.pl
automark.pl
ftest.sh
ftestall.sh
gentest.pl
gentest.sh
qtest.sh
runqtest.pl
runtest.pl
runtest.sh
standardize-submission-files.sh
test1.pl
testall.sh

2. 

sh runtest.sh


3.

If need to compare answers for q4 then consult:

['/Users/nicholastimpson/Desktop/AI-Logic/2014-2015/ASGNM1/.automarkrc'].


ailp_start_position(A),q4a(B),answerQ4a(A,B).
ailp_start_position(A),q4b(B),answerQ4b(A,B).
ailp_start_position(A),q4c(B),answerQ4c(A,B).
ailp_start_position(A),q4d(B),answerQ4d(A,B).

ailp_start_position(A),q4a(B),reverse(B,C),answerQ4a(A,C).
ailp_start_position(A),q4b(B),reverse(B,C),answerQ4b(A,C).
ailp_start_position(A),q4c(B),reverse(B,C),answerQ4c(A,C).
ailp_start_position(A),q4d(B),reverse(B,C),answerQ4d(A,C).
