# TinyPTC x11 v0.7.3 Makefile
# Copyright (C) 2000-2002 Alessandro Gatti <a.gatti@tiscali.it>
#
# http://www.sourceforge.net/projects/tinyptc/
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

CFLAGS      = -Wall -Wno-unknown-pragmas -Os
CC          = gcc
CPP         = cpp
SH          = /bin/sh
LFLAGS	    = -s
LD          = gcc
AFLAGS      = -f elf
AS          = nasm
AR          = ar
RANLIB      = ranlib
STRIP	    = strip
SWIG        = swig
RM          = /bin/rm -f
INCLUDES    = -I/usr/X11R6/include -I/usr/local/include -I/usr/include
LIBS        = -L/usr/X11R6/lib -L/usr/lib -L/usr/local/lib -lX11 -lXext -lXxf86vm -lXv
CSOURCES    = test.c
CTARGETS    = xlib.c xdbe.c xshm.c convert.c xvshm.c
ASOURCES    = mmx.s yv12.s
OTARGETS    = $(CTARGETS:.c=.o)
COBJECTS    = $(CSOURCES:.c=.o)
AOBJECTS    = $(ASOURCES:.s=.o)
EXECUTABLE  = tinyptc_test
LIBRARY     = libtinyptc.a

# Feel free to change this.
PYTHON_VERSION = 2.2

all : $(OTARGETS)



$(EXECUTABLE): $(OTARGETS) $(COBJECTS) $(LIBRARY)
	$(LD) $(LDFLAGS) $(OTARGETS) $(COBJECTS) $(AOBJECTS) $(LIBS) -o $(EXECUTABLE)
	$(STRIP) $(EXECUTABLE)

$(LIBRARY): $(OTARGETS) $(AOBJECTS)
	$(AR) cru $(LIBRARY) $(OTARGETS) $(AOBJECTS)
	$(RANLIB) $(LIBRARY)

%.o: %.c
	$(CC) $(CFLAGS) $(INCLUDES) -c $<

# Unfortunately makedepend doesn't work for assembly files...

mmx.i: mmx.h
	$(CPP) $(INCLUDES) mmx.h | $(SH) buildmmx.sh > mmx.i

mmx.o: mmx.s mmx.i
	$(AS) $(AFLAGS) mmx.s

yv12.o: yv12.s
	$(AS) $(AFLAGS) yv12.s

python: $(LIBRARY)
	$(RM) ptctarget.o
	$(CPP) $(INCLUDES) tinyptc.h | $(SH) findused.sh
	$(SWIG) -python tinyptc.h
	$(CC) -c tinyptc_wrap.c -I/usr/include/python$(PYTHON_VERSION) \
	-I/usr/local/include/python$(PYTHON_VERSION) $(INCLUDES)
	$(CC) -shared $(LIBS) $(LIBRARY) convert.o ptctarget.o tinyptc_wrap.o \
	-o _TinyPTC.so

.depend:
	$(RM) .depend 2> /dev/null
	makedepend -f- -- $(INCLUDES) $(CFLAGS) $(CSOURCES) > .depend 2> /dev/null

clean:
	$(RM) $(EXECUTABLE) $(LIBRARY) $(OTARGETS) $(COBJECTS) $(AOBJECTS)
	$(RM) ptctarget.o .depend mmx.i tinyptc_wrap.[co] TinyPTC.py*
	$(RM) _TinyPTC.so

include .depend
