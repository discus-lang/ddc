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

/* #includes */

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

#define __PTC_FROM_SOURCE

#include "xdga1.h"

/* Open the screen */

int
ptc_open (char *title, int width, int height)
{
  /* Check for the effective UID of the running process */
  if (geteuid ())
    {
      return PTC_FAILURE;
    }
  /* Open a display on the current root window */
  ptc_display = XOpenDisplay (NULL);
  if (ptc_display == NULL)
    {
      return PTC_FAILURE;
    }
  /* Get the default screen associated with the previously opened display */
  ptc_screen = DefaultScreen (ptc_display);
  /* Get the default visual */
  ptc_visual = DefaultVisual (ptc_display, ptc_screen);
  /* Get screen bitdepth */
  ptc_depth = DefaultDepth (ptc_display, ptc_screen);
  /* Get a pointer to the supported pixmap formats */
  ptc_pixmap_formats =
    XListPixmapFormats (ptc_display, &ptc_pixmap_format_count);
  /* Check if there's one that's suitable */
  for (ptc_pixmap_counter = 0; ptc_pixmap_counter < ptc_pixmap_format_count;
       ptc_pixmap_counter++)
    {
      if (ptc_depth == ptc_pixmap_formats[ptc_pixmap_counter].depth)
	{
	  /* Set the right value */
	  ptc_converter_depth =
	    ptc_pixmap_formats[ptc_pixmap_counter].bits_per_pixel;
	}
    }
  XFree (ptc_pixmap_formats);
#ifdef __PTC_ENABLE_CONVERSIONS__
  /* Check if a converter is avaliable */
  ptc_convert =
    ptc_request_converter (ptc_converter_depth, ptc_visual->red_mask,
			   ptc_visual->green_mask, ptc_visual->blue_mask);
  if (!ptc_convert)
    {
      /* Close the display */
      XCloseDisplay (ptc_display);
      return 0;
    }
  /* Get the actual bytes-per-pixel value */
  switch (ptc_converter_depth)
    {
    case 8:
      ptc_output_pitch = 1;
      break;
    case 15:
      ptc_output_pitch = 2;
      break;
    case 16:
      ptc_output_pitch = 2;
      break;
    case 24:
      ptc_output_pitch = 3;
      break;
    case 32:
      ptc_output_pitch = 4;
      break;
    }
  /* Allocate the temporary buffer */
  ptc_buffer = (char *) malloc (width * height * ptc_output_pitch);
  if (ptc_buffer == NULL)
    {
      XCloseDisplay (ptc_display);
      return PTC_FAILURE;
    }
#else
  /* It runs only on a 32bpp display if no conversions were activated */
  if (ptc_converter_depth != 32)
    {
      XCloseDisplay (ptc_display);
      return PTC_FAILURE;
    }
  ptc_output_pitch = sizeof (int);
#endif /* __PTC_ENABLE_CONVERSIONS__ */
  /* Check for DGA extension avaliability */
  if (!XF86DGAQueryExtension (ptc_display, &ptc_event_base, &ptc_error_base))
    {
      XCloseDisplay (ptc_display);
      return PTC_FAILURE;
    }
  /* Get framebuffer parameters */
  XF86DGAGetVideo (ptc_display, ptc_screen, &ptc_framebuffer_address,
		   &ptc_framebuffer_width, &ptc_framebuffer_banksize,
		   &ptc_framebuffer_memory);
  /* Check if the mode is banked */
  if (ptc_framebuffer_banksize < ptc_framebuffer_memory)
    {
      XCloseDisplay (ptc_display);
      return PTC_FAILURE;
    }
  /* Check for VidMode extension avaliability */
  if (!XF86VidModeQueryExtension
      (ptc_display, &ptc_event_base, &ptc_error_base))
    {
      XCloseDisplay (ptc_display);
      return PTC_FAILURE;
    }
  /* Get all the mode parameters */
  XF86VidModeGetAllModeLines (ptc_display, ptc_screen, &ptc_totalmodes,
			      &ptc_modeinfodata);
  /* Get current mode parameters */
  XF86VidModeGetModeLine (ptc_display, ptc_screen, &ptc_dotclock,
			  &ptc_currentmodeline);
  /* Search for current mode in parameter list */
  for (ptc_modecounter = 0; ptc_modecounter < ptc_totalmodes;
       ptc_modecounter++)
    {
      if ((ptc_currentmodeline.hdisplay ==
	   ptc_modeinfodata[ptc_modecounter]->hdisplay)
	  && (ptc_currentmodeline.vdisplay ==
	      ptc_modeinfodata[ptc_modecounter]->vdisplay))
	{
	  /* Save the parameters */
	  ptc_previousmode_number = ptc_modecounter;
	  ptc_screen_width = ptc_currentmodeline.hdisplay;
	  ptc_screen_height = ptc_currentmodeline.vdisplay;
	  break;
	}
    }
  ptc_emulate_fullscreen = True;
  /* Search for desired mode in parameter list */
  for (ptc_modecounter = 0; ptc_modecounter < ptc_totalmodes;
       ptc_modecounter++)
    {
      if ((ptc_modeinfodata[ptc_modecounter]->hdisplay == width)
	  && (ptc_modeinfodata[ptc_modecounter]->vdisplay == height))
	{
	  /* Try to switch mode */
	  if (!XF86VidModeSwitchToMode
	      (ptc_display, ptc_screen, ptc_modeinfodata[ptc_modecounter]))
	    {
	      XFree (ptc_modeinfodata);
	      XCloseDisplay (ptc_display);
	      return PTC_FAILURE;
	    }
	  ptc_emulate_fullscreen = False;
	  break;
	}
    }
#ifdef __PTC_BEST_VIDEOMODE__
  /* It works for fullscreen emulation only */
  if (ptc_emulate_fullscreen)
    {
      /* Set up loop */
      ptc_best_delta = INT_MAX;
      ptc_best_videomode = 0;
      /* Check for all avaliable modes */
      for (ptc_modecounter = 0; ptc_modecounter < ptc_totalmodes;
	   ptc_modecounter++)
	{
	  if ((ptc_modeinfodata[ptc_modecounter]->hdisplay >= width)
	      && (ptc_modeinfodata[ptc_modecounter]->vdisplay >= height))
	    {
	      /* If it fits, get delta parameters */
	      ptc_best_x_delta =
		ptc_modeinfodata[ptc_modecounter]->hdisplay - width;
	      ptc_best_x_delta *= ptc_best_x_delta;
	      ptc_best_y_delta =
		ptc_modeinfodata[ptc_modecounter]->vdisplay - height;
	      ptc_best_y_delta *= ptc_best_y_delta;
	      /* Check if the mode was fitting better than the previous one */
	      if (ptc_best_x_delta + ptc_best_y_delta < ptc_best_delta)
		{
		  /* Set up next iteration */
		  ptc_best_delta = ptc_best_x_delta + ptc_best_y_delta;
		  ptc_best_videomode = ptc_modecounter;
		}
	    }
	}
      /* Try to switch mode */
      if (!XF86VidModeSwitchToMode
	  (ptc_display, ptc_screen, ptc_modeinfodata[ptc_best_videomode]))
	{
	  XFree (ptc_modeinfodata);
	  XCloseDisplay (ptc_display);
	  return PTC_FAILURE;
	}
    }
#endif /* __PTC_BEST_VIDEOMODE__ */
  /* Clear event queue */
  XFlush (ptc_display);
  /* Get the root window */
  ptc_root_window = XRootWindow (ptc_display, ptc_screen);
  /* Get exclusive keyboard access */
  XGrabKeyboard (ptc_display, ptc_root_window, False, GrabModeAsync,
		 GrabModeAsync, CurrentTime);
  /* Get exclusive mouse access */
  XGrabPointer (ptc_display, ptc_root_window, True,
		PointerMotionMask | ButtonPressMask | ButtonReleaseMask,
		GrabModeAsync, GrabModeAsync, None, None, CurrentTime);
  /* Leave root mode */
  setuid (getuid ());
  /* Get viewport parameters */
  XF86DGAGetViewPortSize (ptc_display, ptc_screen,
			  (int *) (&ptc_screen_width),
			  (int *) (&ptc_screen_height));
  /* Set fullscreen emulation up */
  if (ptc_emulate_fullscreen)
    {
      /* Get effective framebuffer address */
#ifdef __PTC_ENABLE_CONVERSIONS__
      ptc_framebuffer_start =
	ptc_framebuffer_width * ((ptc_screen_height - height) / 2) *
	ptc_output_pitch +
	((ptc_screen_width - width) / 2) * ptc_output_pitch;
#else
      ptc_framebuffer_start =
	ptc_framebuffer_width * ((ptc_screen_height - height) / 2) *
	sizeof (int) + ((ptc_screen_width - width) / 2) * sizeof (int);
#endif /* __PTC_ENABLE_CONVERSIONS__ */
    }
  else
    {
      /* Ignore offsets */
      ptc_framebuffer_start = 0;
    }
  /* Get the pitch value */
  ptc_framebuffer_pitch = ptc_framebuffer_width * ptc_output_pitch;
  /* Do a safety fork */
  if (XF86DGAForkApp (ptc_screen))
    {
      /* Exit DGA mode */
      XF86DGADirectVideo (ptc_display, ptc_screen, False);
      /* Switch back to the previous video mode */
      XF86VidModeSwitchToMode (ptc_display, ptc_screen,
			       ptc_modeinfodata[ptc_previousmode_number]);
      /* Free modeinfo data */
      XFree (ptc_modeinfodata);
      /* Leave exclusive keyboard access */
      XUngrabKeyboard (ptc_display, CurrentTime);
      /* Leave exclusive mouse access */
      XUngrabPointer (ptc_display, CurrentTime);
      /* Close the display */
      XCloseDisplay (ptc_display);
      return PTC_FAILURE;
    }
  /* Initialize DGA video access */
  XF86DGADirectVideo (ptc_display, ptc_screen,
		      XF86DGADirectGraphics | XF86DGADirectKeyb |
		      XF86DGADirectMouse);
  /* Clear the screen */
  memset (ptc_framebuffer_address, 0x00,
	  ptc_framebuffer_width * ptc_screen_height * ptc_output_pitch);
  /* Select the input events that should be reported */
  XSelectInput (ptc_display, DefaultRootWindow (ptc_display),
		KeyPressMask | KeyReleaseMask);
  /* Set the final viewport */
  XF86DGASetViewPort (ptc_display, ptc_screen, 0, 0);
  /* Clear event queue */
  XFlush (ptc_display);
  /* Save the buffer size */
  ptc_viewport_width = width;
  ptc_viewport_height = height;
  return PTC_SUCCESS;
}

/* Update the screen */

int
ptc_update (void *buffer)
{
  char *ptc_buffer;

  ptc_buffer=(char *)buffer;
  ptc_framebuffer_index = 0;
#ifdef __PTC_ENABLE_CONVERSIONS__
  ptc_source_index = 0;
  ptc_destination_index = 0;
#endif /* __PTC_ENABLE_CONVERSIONS__ */
  /* Do the blit line by line */
  for (ptc_blitcounter = 0; ptc_blitcounter < ptc_viewport_height;
       ptc_blitcounter++)
    {
#ifdef __PTC_ENABLE_CONVERSIONS__
      /* Conversion */
      ptc_convert (ptc_buffer + ptc_source_index,
		   ptc_framebuffer_address + ptc_framebuffer_start +
		   ptc_destination_index, ptc_viewport_width);
      /* Pointers update */
      ptc_source_index += ptc_viewport_width * sizeof (int);
      ptc_destination_index += ptc_framebuffer_pitch;
#else
      /* Copy the data */
      memcpy (ptc_framebuffer_address + ptc_framebuffer_start +
	      ptc_framebuffer_index, ptc_buffer,
	      ptc_viewport_width * sizeof (int));
      ptc_buffer += ptc_viewport_width * sizeof (int);
      /* Set up offsets */
      ptc_framebuffer_index += ptc_framebuffer_pitch;
#endif /* __PTC_ENABLE_CONVERSIONS__ */
    }
  /* Process incoming events */
  if (ptc_process_events ())
    {
#ifdef __PTC_CLEANUP_CALLBACK__
      ptc_cleanup_callback ();
#endif /* __PTC_CLEANUP_CALLBACK__ */
      ptc_close ();
      exit (0);
    }
  return PTC_SUCCESS;
}

/* Process events */

int
ptc_process_events (void)
{
  XEvent ptc_xevent;
  KeySym ptc_keysym;
  /* Check if there are events waiting in the display's queue */
  if (XPending (ptc_display))
    {
      /* Get the next event in queue */
      XNextEvent (ptc_display, &ptc_xevent);
      /* Check if it's a keypress event */
      if (ptc_xevent.type == KeyPress)
	{
	  /* Get the keysym */
	  ptc_keysym = XLookupKeysym (&ptc_xevent.xkey, 0);
	  /* Check if the key pressed was a function one */
	  if ((ptc_keysym >> 8) == __PTC_FUNCTION_KEY__)
	    {
	      /* Check if it was the escape key */
	      if ((ptc_keysym & 0xFF) == __PTC_ESCAPE_KEY__)
		{
		  return PTC_SUCCESS;
		}
	    }
	}
    }
  return PTC_FAILURE;
}

/* Close the screen */

void
ptc_close (void)
{
  /* Exit from DGA mode */
  XF86DGADirectVideo (ptc_display, ptc_screen, False);
  /* Switch back to the previous video mode */
  XF86VidModeSwitchToMode (ptc_display, ptc_screen,
			   ptc_modeinfodata[ptc_previousmode_number]);
  /* Free modeinfo data */
  XFree (ptc_modeinfodata);
  /* Leave exclusive keyboard access */
  XUngrabKeyboard (ptc_display, CurrentTime);
  /* Leave exclusive mouse access */
  XUngrabPointer (ptc_display, CurrentTime);
  /* Close the display */
  XCloseDisplay (ptc_display);
  /* Deallocate the buffer */
  if (ptc_buffer)
    {
      free (ptc_buffer);
    }
}

#endif /* __PTC_XDGA1__ */
