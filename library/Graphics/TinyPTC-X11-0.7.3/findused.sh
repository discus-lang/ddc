#! /bin/sh

# TinyPTC x11 v0.7.3 Chosen rendering target finder
# Copyright (C) 2001-2002 Alessandro Gatti <a.gatti@tiscali.it>
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

# Is this portable?
pragmas=`grep -i "^ *#pragma target" - | expand | sed -e 's/^ *#pragma target *//g'`

rm -f ptctarget.o

for pragma in $pragmas
do
	case $pragmas in
		xlib) target="xlib.o" ;;
		xdbe) target="xdbe.o" ;;
		xdga1) target="xdga1.o" ;;
		xdga2) target="xdga2.o" ;;
		xshm) target="xshm.o" ;;
		xvshm) target="xvshm.o" ;;
	esac
done

if [ target ]
then
	ln -s $target ptctarget.o
fi
