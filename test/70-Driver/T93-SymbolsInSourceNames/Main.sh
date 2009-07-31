#!/bin/sh

# Make sure we detect file names with bad symbols.
#	As we name files and C level symbols after the file name, 
#	We don't want anything with a '-' in it.
#	
#	GHC doesn't allow module names to have symbols either,
#	and we name the module after the file name.
#
bin/ddc -make $1/hello-world.ds

