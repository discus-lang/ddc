
// Primitive operations that sea code uses.
//   These should all be static inlined so we can compile programs without
//   linking against external code. Operations that need manifest object
//   code should be defined somewhere else, and preferably imported
//   by the foreign-function interface.

#ifndef _DDC_Primitive
#define _DDC_Primitive

#include <Disciple.h>
#include <stdio.h>

// Show an integer.
//   TODO: Define in source program to allocate into a heap object.
static inline
string_t _showInt32 (int32_t i)
{       string_t str = malloc(32);
        snprintf(str, 32, "%d", i);
        return str;
}


// Print a string to stdout.
static inline
void _putStr (string_t str)
{       fputs(str, stdout);
}


#endif 
