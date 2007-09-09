/*
 * TinyPTC x11 v0.7.3 X Shared Memory Extension target
 * Copyright (C) 2000-2002 Alessandro Gatti <a.gatti@tiscali.it>
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

#ifndef __PTC_XSHM_H
#define __PTC_XSHM_H

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/extensions/XShm.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include "tinyptc.h"

#ifdef __PTC_XSHM__

#ifdef __TINYPTC_INCLUDED
#error You chose more than one target. Please make your mind and try again.
#endif /* __TINYPTC_INCLUDED */

#define __TINYPTC_INCLUDED
#pragma target xshm

#ifdef __cplusplus
	extern "C" {
#endif

int ptc_open (char *title, int width, int height);
int ptc_update (void *buffer);
void ptc_close (void);
int ptc_process_events (void);

#ifdef __cplusplus
	}
#endif /* __cplusplus */

#endif /* __PTC_XSHM__ */

#endif /* __PTC_XSHM_H */

#ifdef __PTC_FROM_SOURCE

static Display *ptc_display;
static int ptc_screen;
static int ptc_screen_width;
static int ptc_screen_height;
static int ptc_viewport_width;
static int ptc_viewport_height;
static int ptc_depth;
static Window ptc_root_window;
static Visual *ptc_visual;
static char *ptc_buffer;
static int ptc_pixmap_format_count;
static int ptc_pixmap_counter;
static int ptc_converter_depth;
static XPixmapFormatValues *ptc_pixmap_formats;

static Window ptc_window;
static int ptc_x_position;
static int ptc_y_position;
static XSetWindowAttributes ptc_window_attributes;
static XSizeHints ptc_window_sizehints;
static GC ptc_window_gc;
static XImage *ptc_ximage;

static XShmSegmentInfo ptc_shm_segment;

#ifdef __PTC_ENABLE_CONVERSIONS__
static PTC_CONVERTER ptc_convert;
static int ptc_output_pitch;
static int ptc_blitcounter;
static int ptc_source_index;
static int ptc_destination_index;
#endif /* __PTC_ENABLE_CONVERSIONS__ */

#endif /* __PTC_FROM_SOURCE */
