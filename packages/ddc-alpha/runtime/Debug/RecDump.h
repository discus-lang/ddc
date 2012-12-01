// Recursively Dump the state of a runtime object.
//	Used for debugging the runtime system,
//	not usually called from the user program.
//
#ifndef _DDC_Debug_RecDump
#define _DDC_Debug_RecDump

#include "../Runtime.h"
#include <stdio.h>

// Dump an object to a file
void	_recDumpObj	(FILE* file, Obj* obj);

#endif
