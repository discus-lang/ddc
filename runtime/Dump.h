
#ifndef _DDC_Dump
#define _DDC_Dump

#include "Types.h"
#include <stdio.h>

void	_dumpPanic ();

void	_dumpState 
		(FILE* 		file);

void	_dumpHeap 
		( FILE*		file
		, Word8*	heapBase
		, Word8*	heapMax);


#endif
