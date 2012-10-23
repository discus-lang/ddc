
// Primitive operations that sea code uses.
// In future we'll just import these with the FFI.
#include <stdio.h>
#include "Runtime.h"


// Show an integer.
string_t* showInt (int i)
{       string_t* str = malloc(32);
        snprintf(str, 32, "%d", i);
        return str;
}


// Print a string to stdout.
void putStr (string_t* str)
{       fputs(str, stdout);
}


// Print a string to stdout, with a newline.
void putStrLn (string_t* str)
{       fputs(str,  stdout);
        fputs("\n", stdout);
}

