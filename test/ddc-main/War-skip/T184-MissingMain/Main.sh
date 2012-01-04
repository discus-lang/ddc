#!/bin/sh

# Test module has a main() function, need to make sure it generates a C main().
bin/ddc -c $1/Test.ds

if test `grep -c argc $1/Test.ddc.c` -ne 2 ; then
	exit 1
	fi

if test `grep -c argv $1/Test.ddc.c` -ne 2 ; then
	exit 1
	fi

exit 0




