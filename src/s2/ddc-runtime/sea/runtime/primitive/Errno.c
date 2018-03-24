#include <errno.h>
#include <alloca.h>
#include <string.h>
#include "runtime/Primitive.h"


// Get the value of the 'errno' global.
int     ddcPrimErrnoGet ()
{
        return errno;
}


// Get the name of the given 'errno' value.
Obj*    ddcPrimErrnoShowMessage (int errno_val)
{
        // Write message into a temporary buffer.
        char* pBuf      = alloca(1024);
        strerror_r(errno_val, pBuf, 1024);

        // Allocate a new vector to hold the message.
        int lenActual   = strlen(pBuf);
        Obj* pVec       = ddcPrimVectorAlloc8(lenActual + 1);
        string_t* pPay  = (string_t*)ddcPrimVectorPayload8(pVec);

        // Copy data into the new vector.
        strncpy(pPay, pBuf, lenActual);

        // Ensure there's a null character on the end of the string.
        *(pPay + lenActual) = 0;

        return pVec;
}

