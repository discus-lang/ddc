/*
 * TinyPTC x11 v0.7.3 X Direct Graphics Access Extension v1 target
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

#ifndef __PTC_XDGA1_H
#define __PTC_XDGA1_H

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/extensions/xf86dga.h>
#include <X11/extensions/xf86vmode.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <unistd.h>
#include <sys/types.h>
#include "tinyptc.h"

#ifdef __PTC_XDGA1__

#ifdef __TINYPTC_INCLUDED
#error You chose more than one target. Please make your mind and try again.
#endif /* __TINYPTC_INCLUDED */

#define __TINYPTC_INCLUDED
#pragma target xdga1

#ifdef __cplusplus
	extern "C" {
#endif /* __cplusplus */

int ptc_open (char *title, int width, int height);
int ptc_update (void *buffer);
void ptc_close (void);
int ptc_process_events (void);

#ifdef __cplusplus
	}
#endif /* __cplusplus */

#endif /* __PTC_XDGA1__ */

#endif /* __PTC_XDGA1_H */

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
XPixmapFormatValues *ptc_pixmap_formats;

static char *ptc_framebuffer_address;
static int ptc_framebuffer_width;
static int ptc_framebuffer_banksize;
static int ptc_framebuffer_memory;
static int ptc_framebuffer_start;
static int ptc_framebuffer_pitch;
static int ptc_framebuffer_index;
static int ptc_totalmodes;
static int ptc_dotclock;
static int ptc_modecounter;
static int ptc_previousmode_number;
static int ptc_event_base;
static int ptc_error_base;
static int ptc_blitcounter;
static int ptc_output_pitch;
static Bool ptc_emulate_fullscreen;
static XF86VidModeModeInfo **ptc_modeinfodata;
static XF86VidModeModeLine ptc_currentmodeline;

#ifdef __PTC_BEST_VIDEOMODE__
static int ptc_best_x_delta;
static int ptc_best_y_delta;
static int ptc_best_videomode;
static int ptc_best_delta;
#endif /* __PTC_BEST_VIDEOMODE__ */

#ifdef __PTC_ENABLE_CONVERSIONS__
PTC_CONVERTER ptc_convert;
static int ptc_source_index;
static int ptc_destination_index;
#endif /* __PTC_ENABLE_CONVERSIONS__ */

#endif /* __PTC_FROM_SOURCE */
