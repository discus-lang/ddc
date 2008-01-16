/*
 * TinyPTC x11 v0.7.3 Example file
 * Copyright (C) 2001-2002 Alessandro Gatti <a.gatti@tiscali.it>
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

#include "tinyptc.h"

#include <stdio.h>

#define WIDTH 320
#define HEIGHT 200
#define SIZE WIDTH*HEIGHT

static int noise;
static int carry;
static int counter;
static int seed = 0x12345;
static int pixel[SIZE];

void
ptc_cleanup_callback (void)
{
  fprintf (stderr, "Callback!\n");
}

int
main ()
{
  if (!ptc_open ("test", WIDTH, HEIGHT))
    return 1;
  for (;;)
    {
      for (counter = 0; counter < SIZE; counter++)
	{
	  noise = seed;
	  noise >>= 3;
	  noise ^= seed;
	  carry = noise & 1;
	  noise >>= 1;
	  seed >>= 1;
	  seed |= (carry << 30);
	  noise &= 0xFF;
	  pixel[counter] = (noise << 16) | (noise << 8) | noise;
	}
      ptc_update (pixel);
    }
  ptc_close ();
  return 0;
}
