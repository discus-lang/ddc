#!/bin/bash

# Get the architecture of the machine.
# ZOMG, pain. We have to trim new lines on Cygwin for this to work. 
ghc --make make/config/goop/getBits.hs -o make/config/goop/getBits > /dev/null 2>&1
bits=`make/config/goop/getBits | sed -e 's/ *$//'`

# Get the name of the cpu.
cpu=`uname -m`

# Normalise the name of the architecture we're on.
case "$cpu-$bits" in
	i?86-32)
		echo "x86"
		;;
	x86_64-64)
		echo "x86_64"
		;;
	x86_64-32)
		echo "x86"
		;;
	ppc64-32)
		echo "ppc"
		;;
	*)
		echo "unknown"
		;;
	esac

