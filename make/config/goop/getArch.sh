#!/bin/bash

# Get the name of the cpu.
cpu=`uname -m`

# Normalise the name of the architecture we're on.
case "$cpu" in
	i?86)   echo "x86"
		;;
	x86_64)
		echo "x86_64"
		;;
	ppc64)
		echo "ppc"
		;;
	*)
		echo "unknown"
		;;
	esac

