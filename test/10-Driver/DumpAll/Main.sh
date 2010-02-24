#!/bin/sh

# Check Options parsing makes files provided without the --make option
touch $1/Main.ds
bin/ddc $1/Main.ds -o $1/Main.bin -dump
$1/Main.bin