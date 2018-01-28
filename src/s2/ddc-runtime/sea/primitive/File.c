#pragma once
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include "Runtime.h"


// -- File --------------------------------------------------------------------
// Read the contents of a file into a string.
Obj* ddcPrimFileRead (string_t* path)
{
        int fd          = open (path, O_RDONLY);
        if (fd == -1) {
                printf("primFileRead: cannot open %s\n", path);
                abort();
        }

        off_t lenBuf    = lseek (fd, 0, SEEK_END);
        lseek(fd, 0, SEEK_SET);

        Obj* pObj       = ddcAllocRaw (0, 4 + lenBuf + 1);
        uint8_t* p8     = _ddcPayloadRaw(pObj);
        uint32_t* pLen  = (uint32_t*)p8;
        string_t* pStr  = (string_t*)(p8 + 4);

        uint32_t  nRead = 0;
        for(;;) {
                ssize_t lenRead = read (fd, pStr, lenBuf);
                if (lenRead == 0) break;
                nRead += lenRead;
                pStr  += lenRead;
        }

        *pStr           = 0;
        *pLen           = nRead + 1;

        close (fd);
        return pObj;
}

