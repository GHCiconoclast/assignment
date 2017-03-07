#!/bin/sh

userdir=`pwd`
scriptlocation=`dirname $0`
librarylocation="$userdir/$scriptlocation/ailp/library"
automarklocation="$librarylocation/assignment1_automark.pl"

# define a directory with submissions or a single file
if [ -z "$1"  ] ; then
  echo "Please supply and argument containing a path to a folder with submissions"
  echo "    \`./runtest.sh ./ASGNM1\`"
  echo "Or just a single submission file"
  echo "    \`./runtest.sh ./ASGNM1/12345/assignment1_12345.pl\`"
  echo "You can also check particular question"
  echo "    \`./runtest.sh ./ASGNM1 question\` or"
  echo "    \`./runtest.sh ./ASGNM1/12345/assignment1_12345.pl question\`"
  echo "\nAlternatively you can generate tests"
  echo "    \`./runtest.sh gentest\`"
  echo "or generate csv file for FEN submission"
  echo "    \`./runtest.sh ./ASGNM1 safe\`"
  exit
fi
subs="$1"

# generate tests if asked
if [ "$1" == "gentest" ] ; then
  swipl -g "['$automarklocation'], gentest, halt." -t "halt(1)"
  exit
fi

# if asked generate csv for FEN upload
if [ "$2" == "safe" ] ; then
  if [ -d "$subs" ] ; then
    echo "Student, Asgnm1, Feedback" > Asgnm1_safe.csv
    for student in $subs/* ; do
      coms=$(cat $student/comments.txt)
      mark=`echo "$coms" | grep "total: " | sed 's/total:.\([0-9]\{1,2\}\)\/25.*/\1/'`
      if [ -z $mark ] ; then
        mark=0
      fi
      echo "${student##*/}, $mark, \"${coms//\"/\"\"}\"" >> Asgnm1_safe.csv
    done
  else
    echo "$subs is not a directory!"
  fi
  exit
fi

# check if directory
if [ -d "$subs" ] ; then
  for student in $subs/* ; do
    echo ======
    echo ${student##*/}
    echo ======
    cd $student
    # check for a file: assignment1_xxxxx.pl
    userfile="assignment1_${student##*/}.pl"
    if [ ! -e "$userfile" ] ; then
      echo "$userfile does not exist"
      userfile=`find -E . -regex '\.(/daylate/|/weeklate/|/)assig(nm|m)ent1_[0-9]{5}\.pl'`
      if [ ! -e "$userfile" ] ; then
        echo "could not locate submission"
        echo "Which of the available files would you like to use:"
        echo =~=~=~=~=~=
        ls
        echo =~=~=~=~=~=
        while [ ! -e "$userfile" ] && [ "next" != "$userfile" ] ; do
          printf "Please input a correct filename\n> "
          read userfile
        done
        echo "Checking $userfile"
      else
        echo "$userfile found"
      fi
    fi

    # link the file to default name
    if [ -e "ailp_assignment1.pl" ] ; then
      echo "\`ailp_assignment1.pl\` already exists!"
    else
      ln -s "$userfile" "ailp_assignment1.pl"
    fi

    # check if already automarked
    if [ -e ./am ]; then
      echo "*** Already automarked ***"
    else
      # check for local copy of runtest.pl
      if [ -e "./runtest.pl" ] ; then
        echo "*** Local runtest.pl detected ***";
        swipl -g "assert(am_student(${student##*/}))" -f ./runtest.pl >& stats.txt
      else
        swipl -g "assert(am_student(${student##*/})), ['$automarklocation'], test, halt. %(test->halt ; writes(['Automatic testing failed.',nl]),told,halt)." -t 'halt(1)' >& stats.txt
      fi
      touch am
      # check candidate number
      candidate_number=$(swipl -g "candidate_number(X), write(X), halt." -f ailp_assignment1.pl -t 'halt(1)' 2> /dev/null)
      file_candidate=`echo "$userfile" | sed 's/.*_\([0-9]\{5\}\)\.pl/\1/'`
      if [ ${student##*/} == "$candidate_number" ] && [ "$candidate_number" == "$file_candidate" ] ; then
        true
      else
        errcan="FEN candidate number ${student##*/}; candidate_number/1 predicate $candidate_number; and file name $file_candidate do not agree!\n"
        echo "$errcan" >> comments.txt
        echo "$errcan" >> stats.txt
        echo "$errcan"
        sleep 5
      fi
    fi

    cd $userdir
  done
  # check if file
elif [ -e "$subs" ] ; then
  parent=`dirname $subs`
  cd $parent

  # extract candidate number
  candidate=`echo "$parent" | sed 's/.*\([0-9]\{5\}\)/\1/'`

  # link the file to default name
  if [ -e "ailp_assignment1.pl" ] ; then
    echo "\`ailp_assignment1.pl\` already exists!"
  else
    ln -s $(basename $subs) ailp_assignment1.pl
  fi

  # check if already automarked
  if [ -e ./am ]; then
    echo "*** Already automarked ***"
  else
    # check only one question
    if [ "$2" == "question" ] ; then
      # check for local copy of runqtest.pl
      if [ -e "./runqtest.pl" ] ; then
        echo "*** Local runqtest.pl detected ***";
        swipl -g "assert(am_student($candidate))" -f ./runqtest.pl
      else
        swipl -g "['$automarklocation'], qtest, halt. %(test->halt ; writes(['Automatic testing failed.',nl]),told,halt)." -t 'halt(1)'
      fi
    # check the whole file
    else
      # check for local copy of runtest.pl
      if [ -e "./runtest.pl" ] ; then
        echo "*** Local runtest.pl detected ***";
        swipl -g "assert(am_student($candidate))" -f ./runtest.pl >& stats.txt
      else
        swipl -g "assert(am_student($candidate)), ['$automarklocation'], test, halt. %(test->halt ; writes(['Automatic testing failed.',nl]),told,halt)." -t 'halt(1)' >& stats.txt
      fi
    fi
    touch am
    # check candidate number
    candidate_number=$(swipl -g "candidate_number(X), write(X), halt." -f ailp_assignment1.pl -t 'halt(1)' 2> /dev/null)
    file_candidate=`echo $(basename $subs) | sed 's/.*_\([0-9]\{5\}\)\.pl/\1/'`
    if [ "$candidate" == "$candidate_number" ] && [ "$candidate_number" == "$file_candidate" ] ; then
      true
    else
      errcan="FEN candidate number $candidate; candidate_number/1 predicate $candidate_number; and file name $file_candidate do not agree!\n"
      echo "$errcan" >> comments.txt
      echo "$errcan" >> stats.txt
      echo "$errcan"
      sleep 5
    fi
  fi
  cd $userdir
  # neither file nor directory
else
  echo "$subs is neither a directory nor a file."
  exit
fi
