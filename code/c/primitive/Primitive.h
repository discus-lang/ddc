// Primitive operations that generated C code uses.
//   These should all be static inlined so we can compile programs without
//   linking against external code. Operations that need manifest object
//   code should be defined somewhere else, and preferably imported
//   by the foreign-function interface.
#pragma once
#include <stdio.h>
#include "Runtime.h"

// Store Primops --------------------------------------------------------------
#define _SIZE(type)                     sizeof(type)

// Read from a field of an Object.
//   We use an explicit macro to make it easier to see what is happening in
//   the generated code.
#define _READ(type,addr,offset)         (*((type *)(addr + offset)))

// Write to a field of an Object.
//   We use an explicit macro to make it easier to see what is happening in
//   the generated code.
#define _WRITE(type,addr,offset,val)    ((*((type *)(addr + offset))) = val)

// Read from a pointer plus offset in bytes.
#define _PEEK(type,ptr,offset)          (*(type *)(((uint8_t *) ptr) + offset))

// Pointer to address conversions.
#define _MAKEPTR(type,addr)             ((type *)addr)
#define _TAKEPTR(type,ptr)              ((addr_t)ptr)
#define _CASTPTR(dstType,srcType,ptr)   ((dstType*)ptr)


// Other primitives -----------------------------------------------------------
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
