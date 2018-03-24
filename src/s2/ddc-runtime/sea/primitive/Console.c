#pragma once
#include <stdio.h>
#include "Runtime.h"


// -- Stdin -------------------------------------------------------------------
// Read a string from stdin and pack it into a vector of characters.
Obj* ddcPrimStdinGetVector (nat_t lenMax)
{
        string_t* pBuf  = alloca (lenMax);
        pBuf            = fgets (pBuf, lenMax, stdin);
        if (pBuf == NULL) {
                printf("ddc-runtime.ddcPrimStdinGetVector: failed\n");
                abort();
        }

        // Allocate a new vector to hold the read text.
        nat_t lenActual = strlen(pBuf);
        Obj* pVec       = ddcPrimVectorAlloc8(lenActual + 1);
        string_t* pPay  = (string_t*)ddcPrimVectorPayload8(pVec);

        // Copy data into the new vector.
        strncpy(pPay, pBuf, lenActual);

        // Ensure there's a null character on the end of the string.
        *(pPay + lenActual) = 0;

        return pVec;
}


// -- Stdout ------------------------------------------------------------------
// Print a C string to stdout.
void ddcPrimStdoutPutString (string_t* str)
{       fputs(str, stdout);
        fflush(stdout);
}

// Print a text literal to stdout.
void ddcPrimStdoutPutTextLit (string_t* str)
{       fputs(str, stdout);
        fflush(stdout);
}

// Print a text vector to stdout.
void ddcPrimStdoutPutVector (Obj* obj)
{       string_t* str = (string_t*) (_ddcPayloadRaw(obj) + 4);
        fputs(str, stdout);
        fflush(stdout);
}

// Flush stdout.
void ddcPrimStdoutFlush (Obj* obj)
{       fflush(stdout);
}


// -- Stderr ------------------------------------------------------------------
// Print a C string to stderr.
// Use this when printing an error from the runtime system.
void ddcPrimFailString(string_t* str)
{       fputs(str, stderr);
        fflush(stderr);
}

// Print a natural number to stderr.
// Use this when printing an error from the runtime system.
void ddcPrimFailNat(nat_t x)
{       fprintf(stderr, "%lu", x);
        fflush(stderr);
}
