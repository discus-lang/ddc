#!/bin/sh

if test $# -eq 1 ; then
	loc=$1

elif test $# -eq 2 ; then
	loc=$1

elif test -f Main.ds ; then
	loc=`pwd`
else
	echo "Need a directory parameter."
	exit 1
	fi

(cd $loc && make clean check)

