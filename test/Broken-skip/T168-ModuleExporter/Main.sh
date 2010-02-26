#!/bin/sh

bin/ddc -c $1/Foo.ds || exit 1

grep -v foreign $1/Foo.di > $1/Foo.stripped.di

count=`grep -c project_ $1/Foo.stripped.di`
if test $count -gt 1 ; then
	echo "Error, only one projection should be exported."
	exit 1
	fi

exit 0
