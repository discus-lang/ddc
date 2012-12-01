// Dump the state of the runtime system and/or heap.
//	Used for debugging the runtime system, 
//	not usually called from the user program.
//
#ifndef _DDC_Debug_Dump
#define _DDC_Debug_Dump

#include "../Runtime.h"
#include <stdio.h>

// Dump the whole RTS state to a file.
//	Then abort the program.
void	_dumpPanic ();

// Dump the RTS state, not in including the heap.
void	_dumpState 	(FILE* 	file);

// Dump the heap contents to a file.
void	_dumpHeap 
		( FILE*		file
		, Word8*	heapBase
		, Word8*	heapMax);

// Dump an object to a file
void	_dumpObj	(FILE* file, Obj* obj);

// Dump an object and its pretty printed pointer to a file.
void	_dumpObjP	(FILE* file, Obj* obj);

// Dump the contents of various objects.
void	_dumpThunk	(FILE* file, Obj* obj);
void	_dumpSusp	(FILE* file, Obj* obj);
void	_dumpData	(FILE* file, Obj* obj);
void	_dumpDataR	(FILE* file, Obj* obj);
void	_dumpDataRS	(FILE* file, Obj* obj);
void	_dumpDataM	(FILE* file, Obj* obj);

#endif
