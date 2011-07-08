// Make CAFs out of #defines in <stdio.h>

#include "../../Types.h"
#include "StdIO.h"

#include <stdio.h>

Int32    _runtimeCAF_stdio_seek_set = SEEK_SET;
Int32    _runtimeCAF_stdio_seek_cur = SEEK_CUR;
Int32    _runtimeCAF_stdio_seek_end = SEEK_END;

