
// Primitive operations that generated C code uses.
//   These should all be static inlined so we can compile programs without
//   linking against external code. Operations that need manifest object
//   code should be defined somewhere else, and preferably imported
//   by the foreign-function interface.

#ifndef _DDC_Primitive
#define _DDC_Primitive

#include <stdio.h>
#include "Runtime.h"

// Show an integer.
//   TODO: This should be defined in an external module.
static inline
string_t _showInt (int_t i)
{       string_t str = malloc(32);
        snprintf(str, 32, "%d", i);
        return str;
}


// Print a string to stdout.
//   TODO: This should really be imported via the FFI.
static inline
void _putStr (string_t str)
{       fputs(str, stdout);
}


// Print a string to stdout, with a newline.
//   TODO: This should really be imported via the FFI.
static inline
void _putStrLn (string_t str)
{       fputs(str,  stdout);
        fputs("\n", stdout);
}


#endif 
