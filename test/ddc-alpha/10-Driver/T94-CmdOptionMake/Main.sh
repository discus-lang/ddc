#!/bin/sh

# Check Options parsing makes files provided without the --make option
touch $1/Main.ds
cp $1/Main.ds $2/Main.ds
bin/ddc-alpha $2/Main.ds -o $2/hello.bin
$2/hello.bin
