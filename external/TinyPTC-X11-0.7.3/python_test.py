#! /usr/bin/env python

# TinyPTC x11 v0.7.3 Python example
# Copyright (C) 2002 Alessandro Gatti <a.gatti@tiscali.it>
#
# http://www.sourceforge.net/projects/tinyptc/
#
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation; either version 2 of the License, or (at your
# option) any later version.
#
# This library is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
# for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

import TinyPTC

WIDTH       = 320
HEIGHT      = 200
SIZE        = (WIDTH * HEIGHT)
noise       = None
carry       = None
counter     = None
pixel       = None
seed        = 0x12345

if not TinyPTC.ptc_open("Test", 320, 200):
	raise Exception, "Can't open window!"

pixel = TinyPTC.pixel_array(SIZE)

while True:
	for counter in range(SIZE):
		noise = seed
		noise = noise >> 3
		noise = noise ^ seed
		carry = noise & 1
		noise = noise >> 1
		seed = seed >> 1
		seed = seed | (carry << 30)
		noise = noise & 0xFF
		pixel[counter] = (noise << 16) | (noise << 8) | noise
	TinyPTC.ptc_update(pixel)

TinyPTC.ptc_close()
