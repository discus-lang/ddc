#!/bin/bash

ghc --make make/bits.hs -o make/bits > /dev/null 2>&1
bits=`make/bits`
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

