#!/bin/sh

bin/ddc -c $1/Foo.ds || exit 1

cmp $1/Foo.di $1/Foo.di.expected > /dev/null

if test $? -ne 0 ; then
	echo "Error."
	exit 1
	fi

