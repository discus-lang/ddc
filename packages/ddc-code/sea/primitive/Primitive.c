
// Primitive operations that sea code uses.
// In future we'll just import these with the FFI.
#include <stdio.h>
#include "Runtime.h"


// Show an integer.
// This leaks the space for the string, but nevermind until we get a GC.
string_t* primShowInt (int i)
{       string_t* str = malloc(32);
        snprintf(str, 32, "%d", i);
        return str;
}


// Show a natural number.
// This leaks the space for the string, but nevermind until we get a GC.
string_t* primShowNat (nat_t i)
{       string_t* str = malloc(32);
        snprintf(str, 32, "%u", (unsigned int)i);
        return str;
}


// Print a string to stdout.
void primPutString (string_t* str)
{       fputs(str, stdout);
}

