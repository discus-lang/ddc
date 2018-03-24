#pragma once
#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include "Runtime.h"


// Abort the program due to an inexhaustive case match.
//
// When desugaring guards, if the compiler cannot determine that
// the guards are exhaustive then a call to this function is
// inserted as a default case.
//
Obj*    ddcPrimErrorDefault(string_t* source, uint32_t line)
{
        fprintf ( stderr
                , "\nDDC runtime error: inexhaustive case match.\n at: %s:%" PRId32 "\n"
                , source, line);
        exit(1);
        return 0;
}
