#!/bin/sh

if test $# -eq 1 ; then
	srcDir=$1
        buildDir=$srcDir

elif test $# -eq 2 ; then
	loc=$1
        buildDir=$2

elif test -f Main.ds ; then
	loc=`pwd`
        buildDir=`pwd`
else
	echo "Need a directory parameter."
	exit 1
	fi

(cd $loc && make BUILDDIR=$2 clean check)

