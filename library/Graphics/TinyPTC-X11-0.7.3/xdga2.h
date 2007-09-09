/*
 * TinyPTC x11 v0.7.3 X Direct Graphics Access Extension v2 target
 * Copyright (C) 2001-2002 Alessandro Gatti <a.gatti@tiscali.it>
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

#ifndef __PTC_XDGA2_H
#define __PTC_XDGA2_H

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/extensions/xf86dga.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <unistd.h>
#include <sys/types.h>
#include "tinyptc.h"

#ifdef __PTC_XDGA2__

#ifdef __TINYPTC_INCLUDED
#error You chose more than one target. Please make your mind and try again.
#endif /* __TINYPTC_INCLUDED */

#define __TINYPTC_INCLUDED
#pragma target xdga2

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

#endif /* __PTC_XDGA2__ */

#endif /* __PTC_XDGA2_H */

#ifdef __PTC_FROM_SOURCE

static Display *ptc_display;
static int ptc_screen;
static int ptc_screen_width;
static int ptc_screen_height;
static int ptc_viewport_width;
static int ptc_viewport_height;
static Visual *ptc_visual;
static char *ptc_buffer;

static int ptc_framebuffer_start;
static int ptc_framebuffer_index;
static int ptc_dga_event_base;
static int ptc_error_base;
static int ptc_blitcounter;
static int ptc_dga_major_version;
static int ptc_dga_minor_version;

static int ptc_dga_video_length;
static Bool ptc_dga_emulate_fullscreen;

static int ptc_dga_output_pitch;
static char *ptc_dga_framebuffer_address;
static char *ptc_dga_first_page_address;
static char *ptc_dga_second_page_address;
static XDGAMode *ptc_dga_mode;
static XDGADevice *ptc_dga_device;
static Bool ptc_dga_first_page;
static int ptc_dga_modes;
static int ptc_dga_counter;
static int ptc_dga_new_mode;

#ifdef __PTC_ENABLE_CONVERSIONS__
static int ptc_dga_mode_32;
static int ptc_dga_mode_24;
static int ptc_dga_mode_16;
static int ptc_dga_mode_15;
#endif /* __PTC_ENABLE_CONVERSIONS__ */

#ifdef __PTC_BEST_VIDEOMODE__
#ifdef __PTC_ENABLE_CONVERSIONS__
typedef struct
{
  int best_videomode;
  int best_delta;
}
PTC_VIDEO_DATA;

static PTC_VIDEO_DATA ptc_dga_32;
static PTC_VIDEO_DATA ptc_dga_24;
static PTC_VIDEO_DATA ptc_dga_16;
static PTC_VIDEO_DATA ptc_dga_15;
#else
static int ptc_dga_best_videomode;
static int ptc_dga_best_delta;
#endif /* __PTC_ENABLE_CONVERSIONS__ */
static int ptc_dga_best_x_delta;
static int ptc_dga_best_y_delta;
#endif /* __PTC_BEST_VIDEOMODE__ */

#ifdef __PTC_ENABLE_CONVERSIONS__
PTC_CONVERTER ptc_convert;
static int ptc_source_index;
static int ptc_destination_index;
#endif /* __PTC_ENABLE_CONVERSIONS__ */

#endif /* __PTC_FROM_SOURCE */
