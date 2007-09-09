/*
 * TinyPTC x11 v0.7.3 Pixelformat converters
 * Copyright (C) 2000-2002 Alessandro Gatti <a.gatti@tiscali.it>
 * Copyright (C) 2000-2001 Glenn Fiedler <gaffer@gaffer.org>
 *
 * http://www.sourceforge.net/projects/tinyptc/
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */

#ifndef __TINYPTC_CONVERT_H
#define __TINYPTC_CONVERT_H

/* include files */
#include "tinyptc.h"

#define __PTC_CONVERTER_32_TO_32_RGB888
#define __PTC_CONVERTER_32_TO_32_BGR888
#define __PTC_CONVERTER_32_TO_24_RGB888
#define __PTC_CONVERTER_32_TO_24_BGR888
#define __PTC_CONVERTER_32_TO_16_RGB565
#define __PTC_CONVERTER_32_TO_16_BGR565
#define __PTC_CONVERTER_32_TO_16_RGB555
#define __PTC_CONVERTER_32_TO_16_BGR555

#ifdef __cplusplus
	extern "C" {
#endif /* __cplusplus */
			
/* converter function type */
typedef void (*PTC_CONVERTER) (void *src, void *dst, int pixels);

/* converter request */
PTC_CONVERTER ptc_request_converter (int bits, int32 r, int32 g, int32 b);

#ifdef __cplusplus
	}
#endif /* __cplusplus */

#endif /* __TINYPTC_CONVERT_H */
