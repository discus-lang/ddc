/*
 * TinyPTC x11 v0.7.3 MMX-Optimized pixelformat converters
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

#ifndef __TINYPTC_MMX_H
#define __TINYPTC_MMX_H

#include "tinyptc.h"

#define __PTC_MMX_CONVERT_32_TO_32_BGR888
#define __PTC_MMX_CONVERT_32_TO_32_RGB888
#define __PTC_MMX_CONVERT_32_TO_24_BGR888
#define __PTC_MMX_CONVERT_32_TO_24_RGB888
#define __PTC_MMX_CONVERT_32_TO_16_BGR565
#define __PTC_MMX_CONVERT_32_TO_16_RGB565
#define __PTC_MMX_CONVERT_32_TO_16_BGR555
#define __PTC_MMX_CONVERT_32_TO_16_RGB555

#ifdef __cplusplus
	extern "C" {
#endif /* __cplusplus */

#ifdef __PTC_MMX__
void mmx_memcpy (void *d, void *s, int bytes);
#endif /* __PTC_MMX__ */

#ifdef __PTC_MMX_CONVERT_32_TO_32_BGR888
void mmx_convert_32_to_32_bgr888 (void *d, void *s, int pixels);
#endif /* __PTC_MMX_CONVER_32_TO_32_BGR888 */

#ifdef __PTC_MMX_CONVERT_32_TO_32_RGB888
void mmx_convert_32_to_24_rgb888 (void *d, void *s, int pixels);
#endif /* __PTC_MMX_CONVERT_32_TO_32_RGB888 */

#ifdef __PTC_MMX_CONVERT_32_TO_24_BGR888
void mmx_convert_32_to_24_bgr888 (void *d, void *s, int pixels);
#endif /* __PTC_MMX_CONVERT_32_TO_24_BGR888 */

#ifdef __PTC_MMX_CONVERT_32_TO_16_BGR565
void mmx_convert_32_to_16_rgb565 (void *d, void *s, int pixels);
#endif /* __PTC_MMX_CONVERT_32_TO_16_BGR565 */

#ifdef __PTC_MMX_CONVERT_32_TO_16_RGB565
void mmx_convert_32_to_16_bgr565 (void *d, void *s, int pixels);
#endif /* __PTC_MMX_CONVERT_32_TO_16_RGB565 */

#ifdef __PTC_MMX_CONVERT_32_TO_16_BGR555
void mmx_convert_32_to_16_rgb555 (void *d, void *s, int pixels);
#endif /* __PTC_MMX_CONVERT_32_TO_16_BGR555 */

#ifdef __PTC_MMX_CONVERT_32_TO_16_RGB555
void mmx_convert_32_to_16_bgr555 (void *d, void *s, int pixels);
#endif /* __PTC_MMX_CONVERT_32_TO_16_RGB555 */

#ifdef __cplusplus
	}
#endif /* __cplusplus */

#endif /* __TINYPTC_MMX_H */
