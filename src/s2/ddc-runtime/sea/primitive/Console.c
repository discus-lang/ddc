#pragma once
#include <stdio.h>
#include "Runtime.h"


// -- Stdin -------------------------------------------------------------------
// Read a string from stdin and pack it into a vector of characters.
Obj* ddcPrimStdinGetVector (nat_t len)
{
        string_t* pBuf  = alloca (len);
        pBuf            = fgets (pBuf, len, stdin);
        if (pBuf == NULL) {
                printf("ddc-runtime.ddcPrimStdinGetVector: failed\n");
                abort();
        }

        nat_t n         = strlen(pBuf);
        Obj* pObj       = ddcAllocRaw (0, 4 + n + 1);
        uint8_t* p8     = _ddcPayloadRaw(pObj);
        uint32_t* pLen  = (uint32_t*)p8;
        string_t* pStr  = (string_t*)(p8 + 4);

        memcpy(pStr, pBuf, n + 1);
        *pLen           = n;
        return pObj;
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
