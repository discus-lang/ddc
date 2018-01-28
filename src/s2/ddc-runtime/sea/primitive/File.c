#pragma once
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include "Runtime.h"

// ------------------------------------------------------------------------------------------------
// Read the contents of a file into a text vector.
Obj* ddcPrimFileRead (string_t* path)
{
        int fd  = open (path, O_RDONLY);
        if (fd == -1) {
                printf("ddc-runtime.ddcPrimFileRead: open failed on '%s'.\n", path);
                abort();
        }

        off_t lenBuf    = lseek (fd, 0, SEEK_END);
        if (lseek(fd, 0, SEEK_SET) == -1) {
                printf("ddc-runtime.ddcPrimFileRead: seek failed on '%s'.\n", path);
                abort();
        }

        // Payload contains Word32 data size, the data itself, then the null byte.
        Obj* pObj       = ddcAllocRaw (0, 4 + lenBuf + 1);
        uint8_t* p8     = _ddcPayloadRaw(pObj);
        uint32_t* pLen  = (uint32_t*)p8;
        string_t* pStr  = (string_t*)(p8 + 4);

        uint32_t  nRead = 0;
        for(;;) {
                ssize_t lenRead = read (fd, pStr, lenBuf);
                if (lenRead == -1) {
                        printf("ddc-runtime.ddcPrimFileRead: read error on '%s'.\n", path);
                        abort ();
                }

                if (lenRead == 0)
                        break;

                nRead += lenRead;
                pStr  += lenRead;
        }

        *pStr           = 0;
        *pLen           = nRead + 1;

        if (close (fd) == -1) {
                printf("ddc-runtime.ddcPrimFileWrite: close of handle failed on '%s'.\n", path);
                abort ();
        }
        return pObj;
}


// ------------------------------------------------------------------------------------------------
// Write the contents of a text vector to a file.
void ddcPrimFileWrite (string_t* path, Obj* vec)
{
        int fd  = open (path, O_WRONLY);
        if (fd == -1) {
                printf("ddc-runtime.ddcPrimFileWrite: open failed on '%s'.\n", path);
                abort();
        }

        uint8_t* p8     = _ddcPayloadRaw(vec);
        uint32_t* pLen  = (uint32_t*)p8;
        string_t* pStr  = (string_t*)(p8 + 4);

        ssize_t nLenTotal   = (ssize_t)*pLen;
        ssize_t nLenWritten = write(fd, (void*)pStr, nLenTotal);
        if (nLenWritten == -1) {
                printf("ddc-runtime.ddcPrimFileWrite: write failed on '%s'.\n", path);
        }

        if (nLenWritten /= nLenTotal) {
                printf("ddc-runtime.ddcPrimFileWrite: short write on '%s'.\n", path);
                abort ();
        }

        if (close(fd) == -1) {
                printf("ddc-runtime.ddcPrimFileWrite: close of handle failed on '%s'.\n", path);
                abort ();
        }
        return;
}

