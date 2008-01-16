#! /bin/sh

# TinyPTC x11 v0.7.3 MMX-Optimized pixelformat converters include generator
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

cat << EOF

;
; TinyPTC x11 v0.7.3 MMX-optimized pixelformat converters
; Copyright (C) 2001-2002 Alessandro Gatti <a.gatti@tiscali.it>
; Copyright (C) 2001 Glenn Fiedler <gaffer@gaffer.org>
;
; http://www.sourceforge.net/projects/tinyptc/
;
; This library is free software; you can redistribute it and/or
; modify it under the terms of the GNU Lesser General Public
; License as published by the Free Software Foundation; either
; version 2 of the License, or (at your option) any later version.
;
; This library is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
; Lesser General Public License for more details.
;
; You should have received a copy of the GNU Lesser General Public
; License along with this library; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
; 02111-1307 USA
;

;
; Do not change this file, it is automatically generated.
; Modify mmx.h instead and then recreate this file.
;

EOF

# Is this portable?
lines=`grep -i "^ *void *mmx_" - | expand | sed -e 's/^ *void *//g' | sed -e 's/ *(.*//'`

for line in $lines
do
	case $line in
		mmx_memcpy) echo "%define __PTC_MMX__";;
		mmx_convert_32_to_32_bgr888) echo "%define __PTC_MMX_CONVERT_32_TO_32_BGR888";;
		mmx_convert_32_to_32_rgb888) echo "%define __PTC_MMX_CONVERT_32_TO_32_RGB888";;
		mmx_convert_32_to_24_bgr888) echo "%define __PTC_MMX_CONVERT_32_TO_24_BGR888";;
		mmx_convert_32_to_24_rgb888) echo "%define __PTC_MMX_CONVERT_32_TO_24_RGB888";;
		mmx_convert_32_to_16_bgr565) echo "%define __PTC_MMX_CONVERT_32_TO_16_BGR565";;
		mmx_convert_32_to_16_rgb565) echo "%define __PTC_MMX_CONVERT_32_TO_16_RGB565";;
		mmx_convert_32_to_16_bgr555) echo "%define __PTC_MMX_CONVERT_32_TO_16_BGR555";;
		mmx_convert_32_to_16_rgb555) echo "%define __PTC_MMX_CONVERT_32_TO_16_RGB555";;
	esac
done

echo 
echo "; *** end ***"
