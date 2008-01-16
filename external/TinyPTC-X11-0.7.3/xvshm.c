/*
 * TinyPTC x11 v0.7.3 XVideo with Shared Memory Extension target
 * Copyright (C) 2002 Fred Howell <foohoo@shaw.ca>
 * Copyright (C) 2002 Alessandro Gatti <a.gatti@tiscali.it>
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
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <X11/extensions/XShm.h>
#include <X11/extensions/Xvlib.h>
#include "tinyptc.h"

#ifdef __PTC_XVSHM__

#define __PTC_FROM_SOURCE

#include "xvshm.h"

/* Open the screen */

int
ptc_open (char *title, int width, int height)
{
	unsigned int xvver,xvrel,xvreqbase,xvevbase,xverrbase;
	int i,j,k;
	int nAdaptors;
	XvAdaptorInfo *adaptors;
	int nFmts;
	XvImageFormatValues *fmts;

#ifdef __PTC_MMX__
	/* Round down width and height for MMX routine */
	width -= (width % 8);
	height -= (height % 2);
#else
	yuvbuf_init();
#endif

  /* Open a display on the current root window */
  ptc_display = XOpenDisplay (NULL);
  if (ptc_display == NULL)
    {
      return 0;
    }
  /* Get the default screen associated with the previously opened display */
  ptc_screen = DefaultScreen (ptc_display);
  /* Get the default visual */
  ptc_visual = DefaultVisual (ptc_display, ptc_screen);
  ptc_xv_ok = 1;
  if (XvQueryExtension(ptc_display,&xvver,&xvrel,
		&xvreqbase,&xvevbase,&xverrbase) != Success) {
	  ptc_xv_ok = 0;
  }
  if (ptc_xv_ok == 0) return 0;
  /* Check for XShm extension */
  if (!XShmQueryExtension (ptc_display))
    {
      XCloseDisplay (ptc_display);
      return 0;
    }
  /* Get screen dimensions */
  ptc_screen_width = DisplayWidth (ptc_display, ptc_screen);
  ptc_screen_height = DisplayHeight (ptc_display, ptc_screen);
  /* Get the default root window */
  ptc_root_window = DefaultRootWindow (ptc_display);
  /* Initialize window's attribute structure */
  ptc_window_attributes.border_pixel = BlackPixel (ptc_display, ptc_screen);
  ptc_window_attributes.background_pixel =
    BlackPixel (ptc_display, ptc_screen);
  ptc_window_attributes.backing_store = NotUseful;
#ifdef __PTC_CENTER_WINDOW__
  /* Center the window on the screen */
  ptc_x_position = (ptc_screen_width - width) / 2;
  ptc_y_position = (ptc_screen_height - height) / 2;
#else
  /* Dock the window on the top-left corner */
  ptc_x_position = 0;
  ptc_y_position = 0;
#endif /* __PTC_CENTER_WINDOW__ */
  /* Create the window */
  ptc_window =
    XCreateWindow (ptc_display, ptc_root_window, ptc_x_position,
		   ptc_y_position, width, height, 0, ptc_depth, InputOutput,
		   ptc_visual, CWBackPixel | CWBorderPixel | CWBackingStore,
		   &ptc_window_attributes);
  /* Set the window's name */
  XStoreName (ptc_display, ptc_window, title);
  /* Tell the server to report only keypress-related events */
  XSelectInput (ptc_display, ptc_window, KeyPressMask);
  /* Initialize window's sizehint definition structure */
  /* Clear the window */
  XClearWindow (ptc_display, ptc_window);
  /* Put the window on top of the others */
  XMapRaised (ptc_display, ptc_window);
  /* Clear event queue */
  XFlush (ptc_display);
  /* Get the default graphic context */
  ptc_window_gc = DefaultGC (ptc_display, ptc_screen);
  do {
	  if (ptc_xv_ok == 0) break;
	  if (XvQueryAdaptors(ptc_display,ptc_window,&nAdaptors,&adaptors) != Success) {
		  ptc_xv_ok = 0;
		  break;
	  }
	  ptc_xv_ok = 0;
	  for (i = 0; i < nAdaptors; i++) {
		  for (j = 0; j < adaptors[i].num_ports; j++) {
			  if (!(adaptors[i].type && XvInputMask) || !(adaptors[i].type && XvImageMask)) continue;
			  fmts = XvListImageFormats(ptc_display,adaptors[i].base_id + j, &nFmts);
			  if (fmts == NULL) continue;
			  for (k = 0; k < nFmts; k++) {
				  if (fmts[k].id == 0x32315659) {
					  if (fmts[k].guid[0] != 'Y') break;
					  if (fmts[k].guid[1] != 'V') break;
					  if (fmts[k].guid[2] != '1') break;
					  if (fmts[k].guid[3] != '2') break;
					  if (XvGrabPort(ptc_display,adaptors[i].base_id + j,0) == Success) {
					  	ptc_xv_ok = 1;
						ptc_xv_port = adaptors[i].base_id + j;
					  }
					  break;
				  }
			  }
			  XFree(fmts);
			  if (ptc_xv_ok == 1) break;
		  }
		  if (ptc_xv_ok == 1) break;
	  }
	  XvFreeAdaptorInfo(adaptors);
	  if (ptc_xv_ok == 0) {
	      /* Destroy the window */
	      XDestroyWindow (ptc_display, ptc_window);
	      /* Close the display */
	      XCloseDisplay (ptc_display);
	      ptc_xv_ok = 0;
	      return 0;
	  }
  } while (0);
  if (ptc_xv_ok == 0) return 0;
  /* Get a shared segment */
  ptc_shm_segment.shmid =
    shmget (IPC_PRIVATE, width * height + (width * height)/2,
	    IPC_CREAT | 0777);
  /* Save buffer address */
  ptc_shm_segment.shmaddr = shmat(ptc_shm_segment.shmid,0,0);
  /* Put the segment in read/write */
  ptc_shm_segment.readOnly = False;
  /* Attach the segment to the display */
  if (!XShmAttach (ptc_display, &ptc_shm_segment))
    {
      /* Detach the buffer from the segment */
      shmdt (ptc_shm_segment.shmaddr);
      /* Remove the segment */
      shmctl (ptc_shm_segment.shmid, IPC_RMID, 0);
      /* Destroy the window */
      XvUngrabPort(ptc_display,ptc_xv_port,0);
      XDestroyWindow (ptc_display, ptc_window);
      /* Close the display */
      XCloseDisplay (ptc_display);
      ptc_xv_ok = 0;
      return 0;
    }
  ptc_xvimage = XvShmCreateImage(ptc_display,ptc_xv_port,0x32315659,ptc_shm_segment.shmaddr,
		  width,height,&ptc_shm_segment);
  if (ptc_xvimage == NULL) {
      /* Detach the buffer from the segment */
      shmdt (ptc_shm_segment.shmaddr);
      /* Remove the segment */
      shmctl (ptc_shm_segment.shmid, IPC_RMID, 0);
      /* Destroy the window */
      XvUngrabPort(ptc_display,ptc_xv_port,0);
      XDestroyWindow (ptc_display, ptc_window);
      /* Close the display */
      XCloseDisplay (ptc_display);
      ptc_xv_ok = 0;
      return 0;
   }
  ptc_viewport_width = width;
  ptc_viewport_height = height;
  return 1;
}

/* Update the screen */

int
ptc_update (void *buffer)
{
	int wsize,hsize,dum1,dum2,dum3,dum4;
	unsigned char *pin, *py,*pu,*pv,*pfin;
	int strid8,strid2,stridby2;
	Window wndret;
  if (ptc_xv_ok == 1) {
	  if (!XGetGeometry(ptc_display,ptc_window,&wndret,&dum1,&dum2,
				  &wsize,&hsize,&dum3,&dum4)) {
		  wsize = ptc_viewport_width;
		  hsize = ptc_viewport_height;
	  }
	  pin = (unsigned char *)buffer;
	  py = ptc_shm_segment.shmaddr;
	  pu = &py[ptc_viewport_width*ptc_viewport_height];
	  pv = &pu[(ptc_viewport_width*ptc_viewport_height)/4];
	  strid8 = ptc_viewport_width*8;
	  strid2 = ptc_viewport_width*2;
	  stridby2 = ptc_viewport_width/2;
	  pfin = pu;
	  while (py < pfin) {
#ifdef __PTC_MMX__
		convert_yv12_mmx(pin,py,pu,pv,ptc_viewport_width);
#else
		convert_yv12_c(pin,py,pu,pv,ptc_viewport_width);
#endif
		pin += strid8;
		py += strid2;
		pu += stridby2;
		pv += stridby2;
	  }
	  XvShmPutImage(ptc_display,ptc_xv_port,ptc_window,ptc_window_gc,
			ptc_xvimage,0,0,ptc_viewport_width,ptc_viewport_height,0,0,wsize,hsize,True);
	  XFlush(ptc_display);
  }
  if (ptc_process_events ())
    {
      ptc_close ();
      exit (0);
    }
  return 1;
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
				  return 1;
				}
		    }
		}
    }
  return 0;
}

/* Close the screen */

void
ptc_close (void)
{
  if (ptc_xv_ok == 1) {
	  XFree(ptc_xvimage);
  }
  /* Detach the segment from the display */
  XShmDetach (ptc_display, &ptc_shm_segment);
  /* Destroy the XShmImage */
  /* Detach the buffer from the segment */
  shmdt (ptc_shm_segment.shmaddr);
  /* Remove the segment */
  shmctl (ptc_shm_segment.shmid, IPC_RMID, 0);
	  XvUngrabPort(ptc_display,ptc_xv_port,0);
  /* Close the window */
  XDestroyWindow (ptc_display, ptc_window);
  XCloseDisplay (ptc_display);
  /* Deallocate the buffer */
  if (ptc_buffer)
    {
      free (ptc_buffer);
      ptc_buffer = NULL;
    }
  ptc_xv_ok = 0;
}

#ifndef __PTC_MMX__

/* very slow conversion routine - just for reference */
static unsigned int ybufr[256];
static unsigned int ybufg[256];
static unsigned int ybufb[256];
static unsigned char ubuf[512];
static unsigned char vbuf[512];
static unsigned char *uptr;
static unsigned char *vptr;

void yuvbuf_init()
{
	unsigned char c,c2;
	int i,j;

	uptr = &ubuf[255];
	vptr = &vbuf[255];
	for (i = 0, c = 0; i < 256; i++, c++) {
		ybufr[i] = 9798*c;
		ybufg[i] = 19235*c;
		ybufb[i] = 3736*c;
	}
	c2 = 255;
	for (j = 0, c = 0; j < 256; j++, c++) {
		ubuf[j] = ((16122*(c-c2))>>15)+128;
		vbuf[j] = ((25203*(c-c2))>>15)+128;
	}
	c2 = 0;
	for (j = 256, c = 1; j < 511; j++, c++) {
		ubuf[j] = ((16122*(c-c2))>>15)+128;
		vbuf[j] = ((25203*(c-c2))>>15)+128;
	}
}

void inline getyuv2(unsigned char *c, unsigned char *y, unsigned char *u, unsigned char *v)
{
	*y = (ybufr[c[0]] + ybufg[c[1]] + ybufb[c[2]]) >> 15;
	*u = uptr[c[2]-*y];
	*v = vptr[c[0]-*y];
}

void convert_yv12_c(void *expix2, unsigned char *py, unsigned char *pu, unsigned char *pv, int width)
{
	unsigned char *l,*nl;
	unsigned int j;
	unsigned char u1,u2,u3,u4;
	unsigned char v1,v2,v3,v4;
	unsigned int tmp1,tmp2;
	unsigned char *pny;

	l = (unsigned char *)expix2;


	nl = &l[4*width];
	pny = &py[width];


		for (j = 0; j < width; j += 2) {
			getyuv2(l,&py[0],&u1,&v1);
			getyuv2(&l[4],&py[1],&u2,&v2);
			getyuv2(nl,&pny[0],&u3,&v3);
			getyuv2(&nl[4],&pny[1],&u4,&v4);
			tmp1 = u1 + u2 + u3 + u4 + 2;
			tmp2 = v1 + v2 + v3 + v4 + 2;
			pu[0] = (unsigned char)(tmp1 >> 2);
			pv[0] = (unsigned char)(tmp2 >> 2);
			py += 2;
			pny += 2;
			pu ++;
			pv ++;
			l += 8;
			nl += 8;
		} 
}

#endif /* !__PTC_MMX__ */

#endif /* __PTC_XVSHM__ */
