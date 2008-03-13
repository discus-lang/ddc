/*
 * TinyPTC x11 v0.7.3 Main header file
 * Copyright (C) 2000-2002 Alessandro Gatti <a.gatti@tiscali.it>
 * Copyright (C) 2002      Fred Howell <foohoo@shaw.ca>
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

#ifndef __TINYPTC_X11
#define __TINYPTC_X11

#define PTC_FAILURE 0
#define PTC_SUCCESS 1

#define __PTC_FUNCTION_KEY__ 0xFF
#define __PTC_ESCAPE_KEY__ 0x1B

typedef unsigned int int32;
typedef unsigned short int16;
typedef unsigned short short16;
typedef unsigned char char8;

/* This directive enables pixelformat conversions - ignored for the
 * XVideo+Mit-Shm target */

#define __PTC_ENABLE_CONVERSIONS__

/* This directive enables mmx-optimized pixelformat conversions */

/* #define __PTC_MMX__ */

/* This directive enables little-endian pixelformat converters - ignored for
 * the XVideo+Mit-Shm target */

/* #define __PTC_LITTLE_ENDIAN__ */

/* This directive enables a best-fit video-mode choice logic for DGA 1.x and
 * DGA 2.x targets */

#define __PTC_BEST_VIDEOMODE__

/* This directive enables the use of an user-supplied callback that will be
 * triggered upon exit */

/* #define __PTC_CLEANUP_CALLBACK__ */

/* If you uncomment this directive, you'll choose raw xlib video rendering */

#define __PTC_XLIB__

/* If you uncomment this directive, you'll choose X Shared memory extension
 * video rendering */

/* #define __PTC_XSHM__ */

/* If you uncomment this directive, you'll choose X Double buffer extension
 * video rendering */

/* #define __PTC_XDBE__ */

/* If you uncomment this directive, you'll choose X Direct Graphics Access
 * extension (version 1.x) video rendering */

/* #define __PTC_XDGA1__ */

/* If you uncomment this directive, you'll choose X Direct Graphics Access
 * extension (version 2.x) video rendering */

/* #define __PTC_XDGA2__ */

/* If you uncomment this directive, you'll choose XVideo YV12 Accelerated
 * extension video rendering (using shared memory) */

/* #define __PTC_XVSHM__ */

#ifdef __PTC_WINDOWED__
#error __PTC_WINDOWED__ is not valid anymore, use __PTC_XLIB__ instead.
#endif /* __PTC_WINDOWED__ */

#ifdef __PTC_XLIB__
#include "xlib.h"
#endif /* __PTC_XLIB__ */

#ifdef __PTC_XSHM__
#include "xshm.h"
#endif /* __PTC_XSHM__ */

#ifdef __PTC_XDBE__
#include "xdbe.h"
#endif /* __PTC_XDBE__ */

#ifdef __PTC_XDGA1__
#include "xdga1.h"
#endif /* __PTC_XDGA1__ */

#ifdef __PTC_XDGA2__
#include "xdga2.h"
#endif /* __PTC_XDGA2__ */

#ifdef __PTC_XVSHM__
#include "xvshm.h"
#endif /* __PTC_XVSHM__ */

#ifndef __TINYPTC_INCLUDED
#error You should select at least one target. If you did not select \
	anything in order to use the DGA target, use __PTC_XDGA1__ or \
	__PTC_XDGA2__ instead. Please check tinyptc.h settings.
#endif /* __TINYPTC_INCLUDED */

#if defined(__PTC_MMX__) && (! defined(__PTC_ENABLE_CONVERSIONS__))
    #if ! defined(__PTC_XVSHM__)
      #undef __PTC_MMX__
    #endif /* !(__PTC_XVSHM__) */
#endif /* __PTC_MMX__ && (!(__PTC_ENABLE_CONVERSIONS__)) */
	
#ifdef __PTC_ENABLE_CONVERSIONS__
#include "convert.h"
#endif /* __PTC_ENABLE_CONVERSIONS__ */

#ifdef SWIG
%module TinyPTC
%{
%}
%include "carrays.i"
%array_class(int, pixel_array);
#endif /* SWIG */

#ifdef __cplusplus
	extern "C" {
#endif /* __cplusplus */
	
extern int ptc_open (char *title, int width, int height);
extern int ptc_update (void *buffer);
extern void ptc_close (void);
int ptc_process_events (void);

#ifdef __PTC_CLEANUP_CALLBACK__
extern void ptc_cleanup_callback (void);
#endif /* __PTC_CLEANUP_CALLBACK__ */

#ifdef __cplusplus
	}
#endif /* __cplusplus */

#endif /* __TINYPTC_X11 */
