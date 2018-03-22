#pragma once
#include <errno.h>
#include <alloca.h>


// Get the value of the 'errno' global.
int     ddcPrimErrnoGet ()
{
        return errno;
}


// Get the name of the given 'errno' value.
Obj*    ddcPrimErrnoShowMessage (int errno)
{
        char* buf       = alloca(1024);
        strerror_r(errno, buf, 1024);
        int len         = strlen(buf);

        Obj*      pObj  = ddcAllocRaw(0, 4 + len + 1);
        uint8_t*  p8    = _ddcPayloadRaw(pObj);
        uint32_t* pLen  = (uint32_t*)p8;
        string_t* pStr  = (string_t*)(p8 + 4);

        *pLen   = strlen(buf);
        strncpy(pStr, buf, 1024);

        return pObj;
}

