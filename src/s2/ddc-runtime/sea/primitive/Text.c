#pragma once
#include <stdarg.h>
#include <string.h>
#include "Runtime.h"
#include "Primitive.h"


// Print into a freshly allocated DDC TextLit heap object.
//   This is used to construct error messages and the like that can be passed
//   back to the user program.
Obj* ddcTextVecVPrintf(const char* fmt, va_list ap)
{
        // Print string into new allocated temp buffer.
        char* pStr=0;
        vasprintf(&pStr, fmt, ap);

        // Get the length of the printed string, not including the null byte.
        int len = strlen(pStr);

        // Allocate a new vector to hold the text,
        // and get a pointer to the payload.
        Obj* pVec       = ddcPrimVectorAlloc8(len + 1);
        string_t* pPay  = (string_t*)ddcPrimVectorPayload8(pVec);

        // Copy data from the string allocated by 'vasprintf' into the heap object.
        strncpy(pPay, pStr, len);

        // Force there to be a null terminator byte on the end of the string.
        *(pPay + len) = 0;

        // Free the temp buffer.
        free(pStr);

        return pVec;
}


// Print into a freshly allocated DDC TextLit heap object.
//   This is used to construct error messages and the like that can be passed
//   back to the user program.
Obj* ddcTextVecPrintf(const char* fmt, ...)
{
        va_list ap;
        va_start(ap, fmt);
        return ddcTextVecVPrintf(fmt, ap);
}

