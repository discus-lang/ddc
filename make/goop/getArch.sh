#!/bin/bash

# Get the architecture of the machine.
# Must be run from the root of the ddc build tree.

ghc --make make/goop/getBits.hs -o make/goop/getBits > /dev/null 2>&1
bits=`make/goop/getBits`
cpu=`uname -m`

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

