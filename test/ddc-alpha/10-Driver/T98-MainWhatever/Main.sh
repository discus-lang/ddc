#!/bin/sh

cp $1/Whatever.ds $2/Whatever.ds
bin/ddc-alpha $2/Whatever.ds -o $2/whatever.bin
$2/whatever.bin
