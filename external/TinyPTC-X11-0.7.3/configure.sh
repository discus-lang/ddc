#!/bin/sh

# On Debian, Ubuntu and derived Linux distributions, these header files are in
# the package x11proto-xext-dev. Other Linux distributions probably have them
# in a similarly named package.
#
# On Mac OS X these header files get installed during the standard installation
# of the Apple XCode development environment.

#===============================================================================
# Header files to look for. For each of these file pairs, the one with a file
# name starting with X is the older deprecated file name.

XDBE="X11/extensions/Xdbe.h"
DBE="X11/extensions/dbe.h"

XSHM="X11/extensions/XShm.h"
SHM="X11/extensions/shm.h"

# Initialize variables.
HAVE_X11_EXTENSIONS_XDBE_H=0
HAVE_X11_EXTENSIONS_XSHM_H=0
HAVE_X11_EXTENSIONS_DBE_H=0
HAVE_X11_EXTENSIONS_SHM_H=0
errors=0

# Search for files.
for loc in /usr/local/include /usr/include ; do
	if test -f $loc/$DBE ; then
		HAVE_X11_EXTENSIONS_DBE_H=1
	elif test -f $loc/$XDBE ; then
		HAVE_X11_EXTENSIONS_XDBE_H=1
		fi

	if test -f $loc/$SHM ; then
		HAVE_X11_EXTENSIONS_SHM_H=1
	elif test -f $loc/$XSHM ; then
		HAVE_X11_EXTENSIONS_XSHM_H=1
		fi
	done

if test "$HAVE_X11_EXTENSIONS_XDBE_H:$HAVE_X11_EXTENSIONS_DBE_H" = "0:0" ; then
	echo "Error : Could not find include file <$DBE> or <$XDBE>."
	errors=1
	fi

if test "$HAVE_X11_EXTENSIONS_XSHM_H:$HAVE_X11_EXTENSIONS_SHM_H" = "0:0" ; then
	echo "Error : Could not find include file <$SHM> or <$XSHM>."
	errors=1
	fi

if test $errors -ne 0 ; then
	exit 1
	fi

if test "$HAVE_X11_EXTENSIONS_XDBE_H:$HAVE_X11_EXTENSIONS_DBE_H" = "1:1" ; then
	# If we have both, disable the older version of the file.
	HAVE_X11_EXTENSIONS_XDBE_H=0
	fi

if test "$HAVE_X11_EXTENSIONS_XSHM_H:$HAVE_X11_EXTENSIONS_SHM_H" = "1:1" ; then
	# If we have both, disable the older version of the file.
	HAVE_X11_EXTENSIONS_XSHM_H=0
	fi

# Generate the config file.
cat > config.h << DEFINES

/* Auto generated file. */

#define  HAVE_X11_EXTENSIONS_XDBE_H		$HAVE_X11_EXTENSIONS_XDBE_H
#define  HAVE_X11_EXTENSIONS_XSHM_H		$HAVE_X11_EXTENSIONS_XSHM_H

#define  HAVE_X11_EXTENSIONS_DBE_H		$HAVE_X11_EXTENSIONS_DBE_H
#define  HAVE_X11_EXTENSIONS_SHM_H		$HAVE_X11_EXTENSIONS_SHM_H

DEFINES


