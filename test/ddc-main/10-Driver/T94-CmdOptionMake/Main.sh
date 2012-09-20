#!/bin/sh

# Check Options parsing makes files provided without the --make option
touch $1/Main.ds
bin/ddc-alpha $1/Main.ds -o $1/hello.bin
$1/hello.bin
