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

/* #includes */

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

#define __PTC_FROM_SOURCE

#include "xdga2.h"

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

  /* Check for DGA extension avaliability */
  if (!XDGAQueryExtension (ptc_display, &ptc_dga_event_base, &ptc_error_base))
    {
      XCloseDisplay (ptc_display);
      return PTC_FAILURE;
    }
  /* Check for version 2.x of the extension */
  ptc_dga_major_version = 0;
  if (!XDGAQueryVersion
      (ptc_display, &ptc_dga_major_version, &ptc_dga_minor_version)
      || (ptc_dga_major_version < 2))
    {
      XCloseDisplay (ptc_display);
      return PTC_FAILURE;
    }
  /* Enable framebuffer access */
  if (!XDGAOpenFramebuffer (ptc_display, ptc_screen))
    {
      XCloseDisplay (ptc_display);
      return PTC_FAILURE;
    }
  /* Get all the modelines */
  ptc_dga_mode = XDGAQueryModes (ptc_display, ptc_screen, &ptc_dga_modes);
  ptc_dga_emulate_fullscreen = True;
#ifdef __PTC_ENABLE_CONVERSIONS__
  ptc_dga_mode_32 = INT_MIN;
  ptc_dga_mode_24 = INT_MIN;
  ptc_dga_mode_16 = INT_MIN;
  ptc_dga_mode_15 = INT_MIN;
#endif /* __PTC_ENABLE_CONVERSIONS__ */
  ptc_dga_new_mode = INT_MIN;
  /* Search for desired mode in parameter list */
  for (ptc_dga_counter = 0; ptc_dga_counter < ptc_dga_modes;
       ptc_dga_counter++)
    {
      if ((ptc_dga_mode[ptc_dga_counter].viewportWidth == width)
	  && (ptc_dga_mode[ptc_dga_counter].viewportHeight == height))
	{
#ifdef __PTC_ENABLE_CONVERSIONS__
	  /* Get the right videomode for each colordepth */
	  if (ptc_dga_mode[ptc_dga_counter].depth == 24)
	    {
	      if (ptc_dga_mode[ptc_dga_counter].bitsPerPixel == 32)
		{
		  ptc_dga_mode_32 = ptc_dga_counter;
		  ptc_dga_emulate_fullscreen = False;
		}
	      if (ptc_dga_mode[ptc_dga_counter].bitsPerPixel == 24)
		{
		  ptc_dga_mode_24 = ptc_dga_counter;
		  ptc_dga_emulate_fullscreen = False;
		}
	    }
	  if (ptc_dga_mode[ptc_dga_counter].bitsPerPixel == 16)
	    {
	      if (ptc_dga_mode[ptc_dga_counter].depth == 15)
		{
		  ptc_dga_mode_15 = ptc_dga_counter;
		  ptc_dga_emulate_fullscreen = False;
		}
	      if (ptc_dga_mode[ptc_dga_counter].depth == 16)
		{
		  ptc_dga_mode_16 = ptc_dga_counter;
		  ptc_dga_emulate_fullscreen = False;
		}
	    }
#else
	  /* Get the right videomode */
	  if ((ptc_dga_mode[ptc_dga_counter].bitsPerPixel == 32)
	      && (ptc_dga_mode[ptc_dga_counter].depth == 32))
	    {
	      ptc_dga_new_mode = ptc_dga_counter;
	      ptc_dga_emulate_fullscreen = False;
	      break;
	    }
#endif /* __PTC_ENABLE_CONVERSIONS__ */
	}
    }

#ifdef __PTC_ENABLE_CONVERSIONS__
  /* Simple heuristics for getting the best videomode and the right
   * converter for that videomode */
  if (ptc_dga_mode_32 == INT_MIN)
    {
      if (ptc_dga_mode_24 == INT_MIN)
	{
	  if (ptc_dga_mode_16 == INT_MIN)
	    {
	      if (ptc_dga_mode_15 == INT_MIN)
		{
		}
	      else
		{
		  ptc_convert =
		    ptc_request_converter (15,
					   ptc_dga_mode[ptc_dga_mode_15].
					   redMask,
					   ptc_dga_mode[ptc_dga_mode_15].
					   greenMask,
					   ptc_dga_mode[ptc_dga_mode_15].
					   blueMask);
		  if (!ptc_convert)
		    {
		      ptc_dga_new_mode = INT_MIN;
		    }
		  else
		    {
		      ptc_dga_new_mode = ptc_dga_mode_15;
		      ptc_dga_output_pitch = 2;
		    }
		}
	    }
	  else
	    {
	      ptc_convert =
		ptc_request_converter (16,
				       ptc_dga_mode[ptc_dga_mode_16].
				       redMask,
				       ptc_dga_mode[ptc_dga_mode_16].
				       greenMask,
				       ptc_dga_mode[ptc_dga_mode_16].
				       blueMask);
	      if (!ptc_convert)
		{
		  ptc_dga_new_mode = INT_MIN;
		}
	      else
		{
		  ptc_dga_new_mode = ptc_dga_mode_16;
		  ptc_dga_output_pitch = 2;
		}
	    }
	}
      else
	{
	  ptc_convert =
	    ptc_request_converter (24, ptc_dga_mode[ptc_dga_mode_24].redMask,
				   ptc_dga_mode[ptc_dga_mode_24].greenMask,
				   ptc_dga_mode[ptc_dga_mode_24].blueMask);
	  if (!ptc_convert)
	    {
	      ptc_dga_new_mode = INT_MIN;
	    }
	  else
	    {
	      ptc_dga_new_mode = ptc_dga_mode_24;
	      ptc_dga_output_pitch = 3;
	    }
	}
    }
  else
    {
      ptc_convert =
	ptc_request_converter (32, ptc_dga_mode[ptc_dga_mode_32].redMask,
			       ptc_dga_mode[ptc_dga_mode_32].greenMask,
			       ptc_dga_mode[ptc_dga_mode_32].blueMask);
      if (!ptc_convert)
	{
	  ptc_dga_new_mode = INT_MIN;
	}
      else
	{
	  ptc_dga_new_mode = ptc_dga_mode_32;
	  ptc_dga_output_pitch = 4;
	}
    }
#else
  ptc_dga_output_pitch = 4;
#endif /* __PTC_ENABLE_CONVERSIONS__ */

#ifdef __PTC_BEST_VIDEOMODE__
  /* It works for fullscreen emulation only */
  if (ptc_dga_emulate_fullscreen)
    {
      /* Set up loop */
#ifdef __PTC_ENABLE_CONVERSIONS__
      ptc_dga_32.best_delta = INT_MAX;
      ptc_dga_24.best_delta = INT_MAX;
      ptc_dga_16.best_delta = INT_MAX;
      ptc_dga_15.best_delta = INT_MAX;
      ptc_dga_32.best_videomode = INT_MIN;
      ptc_dga_24.best_videomode = INT_MIN;
      ptc_dga_16.best_videomode = INT_MIN;
      ptc_dga_15.best_videomode = INT_MIN;
#else
      ptc_dga_best_delta = INT_MAX;
      ptc_dga_best_videomode = INT_MIN;
#endif /* __PTC_ENABLE_CONVERSIONS__ */
      /* Check for all avaliable modes */
      for (ptc_dga_counter = 0; ptc_dga_counter < ptc_dga_modes;
	   ptc_dga_counter++)
	{
	  if ((ptc_dga_mode[ptc_dga_counter].viewportWidth >= width)
	      && (ptc_dga_mode[ptc_dga_counter].viewportHeight >= height))
	    {
	      ptc_dga_best_x_delta =
		ptc_dga_mode[ptc_dga_counter].viewportWidth - width;
	      ptc_dga_best_x_delta *= ptc_dga_best_x_delta;
	      ptc_dga_best_y_delta =
		ptc_dga_mode[ptc_dga_counter].viewportHeight - height;
	      ptc_dga_best_y_delta *= ptc_dga_best_y_delta;

	      /* Check if the mode fits better than the previous one */

#ifdef __PTC_ENABLE_CONVERSIONS__
	      if (ptc_dga_mode[ptc_dga_counter].depth == 24)
		{
		  if (ptc_dga_mode[ptc_dga_counter].bitsPerPixel == 32)
		    {
		      if (ptc_dga_best_x_delta + ptc_dga_best_y_delta <
			  ptc_dga_32.best_delta)
			{
			  ptc_dga_32.best_delta =
			    ptc_dga_best_x_delta + ptc_dga_best_y_delta;
			  ptc_dga_32.best_videomode = ptc_dga_counter;
			}
		    }
		  if (ptc_dga_mode[ptc_dga_counter].bitsPerPixel == 24)
		    {
		      if (ptc_dga_best_x_delta + ptc_dga_best_y_delta <
			  ptc_dga_24.best_delta)
			{
			  ptc_dga_24.best_delta =
			    ptc_dga_best_x_delta + ptc_dga_best_y_delta;
			  ptc_dga_24.best_videomode = ptc_dga_counter;
			}
		    }
		}
	      if (ptc_dga_mode[ptc_dga_counter].bitsPerPixel == 16)
		{
		  if (ptc_dga_mode[ptc_dga_counter].depth == 15)
		    {
		      if (ptc_dga_best_x_delta + ptc_dga_best_y_delta <
			  ptc_dga_15.best_delta)
			{
			  ptc_dga_15.best_delta =
			    ptc_dga_best_x_delta + ptc_dga_best_y_delta;
			  ptc_dga_15.best_videomode = ptc_dga_counter;
			}
		    }
		  if (ptc_dga_mode[ptc_dga_counter].depth == 16)
		    {
		      if (ptc_dga_best_x_delta + ptc_dga_best_y_delta <
			  ptc_dga_16.best_delta)
			{
			  ptc_dga_16.best_delta =
			    ptc_dga_best_x_delta + ptc_dga_best_y_delta;
			  ptc_dga_16.best_videomode = ptc_dga_counter;
			}
		    }
		}
#else
	      /* Set up next iteration */
	      ptc_dga_best_delta =
		ptc_dga_best_x_delta + ptc_dga_best_y_delta;
	      ptc_dga_best_videomode = ptc_dga_counter;
#endif /* __PTC_ENABLE_CONVERSIONS__ */
	    }
	}
    }

#ifdef __PTC_ENABLE_CONVERSIONS__
  /* Simple heuristics for getting the best videomode and the right
   * converter for that videomode */

  if (ptc_dga_32.best_videomode == INT_MIN)
    {
      if (ptc_dga_24.best_videomode == INT_MIN)
	{
	  if (ptc_dga_16.best_videomode == INT_MIN)
	    {
	      if (ptc_dga_15.best_videomode == INT_MIN)
		{
		}
	      else
		{
		  ptc_convert =
		    ptc_request_converter (15,
					   ptc_dga_mode[ptc_dga_15.
							best_videomode].
					   redMask,
					   ptc_dga_mode[ptc_dga_15.
							best_videomode].
					   greenMask,
					   ptc_dga_mode[ptc_dga_15.
							best_videomode].
					   blueMask);
		  if (!ptc_convert)
		    {
		      ptc_dga_new_mode = INT_MIN;
		    }
		  else
		    {
		      ptc_dga_new_mode = ptc_dga_15.best_videomode;
		      ptc_dga_output_pitch = 2;
		    }
		}
	    }
	  else
	    {

	      ptc_convert =
		ptc_request_converter (16,
				       ptc_dga_mode[ptc_dga_16.
						    best_videomode].redMask,
				       ptc_dga_mode[ptc_dga_16.
						    best_videomode].greenMask,
				       ptc_dga_mode[ptc_dga_16.
						    best_videomode].blueMask);
	      if (!ptc_convert)
		{
		  ptc_dga_new_mode = INT_MIN;
		}
	      else
		{
		  ptc_dga_new_mode = ptc_dga_16.best_videomode;
		  ptc_dga_output_pitch = 2;
		}
	    }
	}
      else
	{

	  ptc_convert =
	    ptc_request_converter (24,
				   ptc_dga_mode[ptc_dga_24.best_videomode].
				   redMask,
				   ptc_dga_mode[ptc_dga_24.best_videomode].
				   greenMask,
				   ptc_dga_mode[ptc_dga_24.best_videomode].
				   blueMask);
	  if (!ptc_convert)
	    {
	      ptc_dga_new_mode = INT_MIN;
	    }
	  else
	    {
	      ptc_dga_new_mode = ptc_dga_24.best_videomode;
	      ptc_dga_output_pitch = 3;
	    }
	}
    }
  else
    {
      ptc_convert =
	ptc_request_converter (32,
			       ptc_dga_mode[ptc_dga_32.best_videomode].
			       redMask,
			       ptc_dga_mode[ptc_dga_32.best_videomode].
			       greenMask,
			       ptc_dga_mode[ptc_dga_32.best_videomode].
			       blueMask);
      if (!ptc_convert)
	{
	  ptc_dga_new_mode = INT_MIN;
	}
      else
	{
	  ptc_dga_new_mode = ptc_dga_32.best_videomode;
	  ptc_dga_output_pitch = 4;
	}
    }
#else
  ptc_dga_output_pitch = 4;
  ptc_dga_new_mode = ptc_dga_best_videomode;
#endif /* __PTC_ENABLE_CONVERSIONS__ */
#endif /* __PTC_BEST_VIDEOMODE__ */

  /* No suitable mode found */
  if (ptc_dga_new_mode == INT_MIN)
    {
      XFree (ptc_dga_mode);
      XCloseDisplay (ptc_display);
      return PTC_FAILURE;
    }
  /* Try to switch mode */
  ptc_dga_device =
    XDGASetMode (ptc_display, ptc_screen, ptc_dga_mode[ptc_dga_new_mode].num);
  if (ptc_dga_device == NULL)
    {
      XFree (ptc_dga_mode);
      XCloseDisplay (ptc_display);
      return PTC_FAILURE;
    }
  /* Clear event queue */
  XFlush (ptc_display);
  /* Leave root mode */
  setuid (getuid ());
  /* Get viewport parameters */
  ptc_screen_width = ptc_dga_device->mode.viewportWidth;
  ptc_screen_height = ptc_dga_device->mode.viewportHeight;
  /* Get framebuffer address */
  ptc_dga_framebuffer_address = (char *)ptc_dga_device->data;


  /* Set fullscreen emulation up */
  if (ptc_dga_emulate_fullscreen)
    {
      /* Get effective framebuffer address */
      ptc_framebuffer_start =
	ptc_dga_device->mode.bytesPerScanline *
	((ptc_screen_height - height) / 2) +
	(((ptc_screen_width - width) / 2) * ptc_dga_output_pitch);
    }
  else
    {
      /* Ignore offsets */
      ptc_framebuffer_start = 0;
    }
  ptc_dga_video_length =
    ptc_dga_device->mode.bytesPerScanline *
    ptc_dga_device->mode.viewportHeight;

  ptc_dga_first_page_address = ptc_dga_framebuffer_address + ptc_framebuffer_start;
  ptc_dga_second_page_address =
    ptc_dga_first_page_address + ptc_dga_video_length;

  /* Set the final viewport */
  XDGASetViewport (ptc_display, ptc_screen, 0, 0, XDGAFlipRetrace);
  /* Select the input events that should be reported */
  XDGASelectInput (ptc_display, ptc_screen, KeyPressMask | KeyReleaseMask);
  /* Clear event queue */
  XFlush (ptc_display);
  /* Clear video memory */
  memset (ptc_dga_device->data, 0x00, ptc_dga_video_length * 2);
  /* Save the buffer size */
  ptc_viewport_width = width;
  ptc_viewport_height = height;
  ptc_dga_first_page = False;
  return PTC_SUCCESS;
}

/* Update the screen */

int
ptc_update (void *buffer)
{
  char *ptc_vram_pointer;

  /* This is here just to keep the c++ compiler happy */
  char *ptc_buffer;

  ptc_buffer=(char *)buffer;

  ptc_framebuffer_index = 0;
#ifdef __PTC_ENABLE_CONVERSIONS__
  ptc_source_index = 0;
  ptc_destination_index = 0;
#endif /* __PTC_ENABLE_CONVERSIONS__ */

  /* Get the right memory area to write into */
  if (ptc_dga_first_page == True)
    {
      ptc_vram_pointer = ptc_dga_first_page_address;
    }
  else
    {
      ptc_vram_pointer = ptc_dga_second_page_address;
    }

  /* Do the blit line by line */
  for (ptc_blitcounter = 0; ptc_blitcounter < ptc_viewport_height;
       ptc_blitcounter++)
    {
#ifdef __PTC_ENABLE_CONVERSIONS__
      /* Conversion */
      ptc_convert (ptc_buffer + ptc_source_index,
		   ptc_vram_pointer + ptc_destination_index,
		   ptc_viewport_width);
      /* Pointers update */
      ptc_source_index += ptc_viewport_width * sizeof (int);
      ptc_destination_index += ptc_dga_device->mode.bytesPerScanline;
#else
      /* Data copy */
      memcpy (ptc_vram_pointer + ptc_framebuffer_index, ptc_buffer,
	      ptc_viewport_width * sizeof (int));
      ptc_buffer += ptc_viewport_width * sizeof (int);
      /* Set up offsets */
      ptc_framebuffer_index += ptc_dga_device->mode.bytesPerScanline;
#endif /* __PTC_ENABLE_CONVERSIONS__ */
    }

  /* Wait for retrace */
  while (XDGAGetViewportStatus (ptc_display, ptc_screen));

  /* Flip the two pages */
  if (ptc_dga_first_page == True)
    {
      XDGASetViewport (ptc_display, ptc_screen, 0, 0, XDGAFlipImmediate);
    }
  else
    {
      XDGASetViewport (ptc_display, ptc_screen, 0,
		       ptc_dga_device->mode.viewportHeight,
		       XDGAFlipImmediate);
    }
  ptc_dga_first_page = !ptc_dga_first_page;

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
  XDGAEvent ptc_dga_event;
  KeySym ptc_keysym;
  XKeyEvent ptc_dga_keyevent;
  /* Check if there are events waiting in the display's queue */
  if (XPending (ptc_display))
    {
      /* Get the next event in queue */
      XNextEvent (ptc_display, (XEvent *) & ptc_dga_event);
      ptc_dga_event.type -= ptc_dga_event_base;

      /* Check if it's a keypress event */
      if (ptc_dga_event.type == KeyPress)
	{
	  /* Translate the event */
	  XDGAKeyEventToXKeyEvent (&ptc_dga_event.xkey, &ptc_dga_keyevent);
	  /* Get the keysym */
	  ptc_keysym = XLookupKeysym (&ptc_dga_keyevent, 0);
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
  /* Switch back to the previous video mode */
  XDGASetMode (ptc_display, ptc_screen, 0);
  /* Disable framebuffer access */
  XDGACloseFramebuffer (ptc_display, ptc_screen);
  /* Close the display */
  XCloseDisplay (ptc_display);
  /* Deallocate the buffer */
  if (ptc_buffer)
    {
      free (ptc_buffer);
    }
}

#endif /* __PTC_XDGA2__ */
