
// Primitive operations that sea code uses.
// In future we'll just import these with the FFI.
#include <stdio.h>
#include "Runtime.h"

// Show a pointer.
string_t* primShowAddr (void* ptr)
{       string_t*  str = malloc(32);
        snprintf(str, 32, "%p", ptr);
        return str;
}


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

// Show a Word64.
string_t* primShowWord64 (uint64_t w)
{       string_t* str = malloc(11);
        snprintf(str, 10, "%#08llx", w);
        return str;
}

// Show a Word32.
string_t* primShowWord32 (uint32_t w)
{       string_t* str = malloc(7);
        snprintf(str, 6, "%#04x", w);
        return str;
}

// Show a Word16.
string_t* primShowWord16 (uint16_t w)
{       string_t* str = malloc(5);
        snprintf(str, 4, "%#02x", w);
        return str;
}

// Show a Word8.
string_t* primShowWord8 (uint8_t w)
{       string_t* str = malloc(4);
        snprintf(str, 3, "%#01x", w);
        return str;
}


// Print a C string to stdout.
void primPutSeaString (string_t* str)
{       fputs(str, stdout);
}

