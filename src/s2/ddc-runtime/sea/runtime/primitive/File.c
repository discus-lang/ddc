
// On Linux we need to define _GNU_SOURCE before including stdio.h
// to expose the vasprintf function. On OSX it is visible by default.
#define _GNU_SOURCE
#include <stdio.h>

#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include "runtime/Primitive.h"
#include "Runtime.h"


// ----------------------------------------------------------------------------
// Throw a file exception.
//   This calls back into user land to construct the appropriate exception
//   value.
void ddcPrimFileFail (const char* fmt, ...)
{
        va_list ap;
        va_start(ap, fmt);
        ddcHookCallExceptionFile
                ( ddcPrimMakeInt(errno)
                , ddcTextVecVPrintf(fmt, ap));
        abort();
}


// ----------------------------------------------------------------------------
// Read the contents of a file into a text vector.
Obj* ddcPrimFileRead (string_t* path)
{
        // Open the file.
        int fd  = open (path, O_RDONLY);
        if (fd == -1)
                ddcPrimFileFail("open failed on file '%s'.", path);

        // Get the file length.
        off_t lenBuf    = lseek (fd, 0, SEEK_END);
        if (lseek(fd, 0, SEEK_SET) == -1)
                ddcPrimFileFail("seek failed on file '%s'.", path);

        // Allocate a new vector to hold the data.
        Obj* pObj       = ddcPrimVectorAlloc8(lenBuf + 1);
        uint8_t* pStr   = ddcPrimVectorPayload8(pObj);
        uint32_t nRead  = 0;
        for(;;)
        {       ssize_t lenRead = read (fd, pStr, lenBuf);
                if (lenRead == -1)
                        ddcPrimFileFail("read failed on file '%s'.", path);

                if (lenRead == 0)
                        break;

                nRead += lenRead;
                pStr  += lenRead;
        }

        // Write the null terminating character.
        *pStr   = 0;

        // Check that we successfully read all the data.
        if (nRead /= lenBuf)
                ddcPrimFileFail("short read on file '%s'.", path);

        // Close the file handle.
        if (close (fd) == -1)
                ddcPrimFileFail("close failed on file '%s'.", path);

        return pObj;
}


// ----------------------------------------------------------------------------
// Write the contents of a text vector to a file.
void ddcPrimFileWrite (string_t* path, Obj* pVec)
{
        // Open the file.
        int fd  = open (path, O_WRONLY);
        if (fd == -1)
                ddcPrimFileFail("open failed on '%s'.", path);

        // Get a pointer to the payload data.
        uint8_t*  pStr  = ddcPrimVectorPayload8(pVec);

        uint8_t* p8     = _ddcPayloadRaw(pVec);
        uint32_t* pLen  = (uint32_t*)p8;

        // Write the data.
        ssize_t nLenTotal   = (ssize_t)*pLen;
        ssize_t nLenWritten = write(fd, (void*)pStr, nLenTotal);
        if (nLenWritten == -1)
                ddcPrimFileFail("write failed on '%s'.", path);

        // Check we managed to write all the data.
        if (nLenWritten /= nLenTotal)
                ddcPrimFileFail("short write on '%s'.", path);

        // Close the file handle.
        if (close(fd) == -1)
                ddcPrimFileFail("close failed on '%s'.", path);
}

