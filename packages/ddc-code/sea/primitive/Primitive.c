
// Primitive operations that sea code uses.
// In future we'll just import these with the FFI.
#include <stdio.h>
#include "Runtime.h"


// Abort the program due to an inexhaustive case match.
// 
// When desugaring guards, if the compiler cannot determine that
// the guards are exhaustive then a call to this function is
// inserted as a default case.
//
Obj*    primErrorDefault(string_t* source, uint32_t line)
{
        fprintf ( stderr
                , "\nDDC runtime error: inexhaustive case match.\n at: %s:%d\n"
                , source, line);
        exit(1);

        return 0;
}

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
void primPutString (string_t* str)
{       fputs(str, stdout);
}


// Print a text literal to stdout.
void primPutTextLit (string_t* str)
{       fputs(str, stdout);
}

// Print a text vector to stdout.
void primPutVector (Obj* obj)
{       string_t* str = (string_t*) _payloadRaw(obj);
        fputs(str, stdout);
}
