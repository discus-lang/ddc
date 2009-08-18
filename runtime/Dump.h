
#ifndef _DDC_Dump
#define _DDC_Dump

#include "Object.h"
#include <stdio.h>

void	_dumpPanic ();

void	_dumpState 	(FILE* 	file);

void	_dumpHeap 
		( FILE*		file
		, Word8*	heapBase
		, Word8*	heapMax);

void	_dumpObj	(FILE* file, Obj* obj);
void	_dumpObjP	(FILE* file, Obj* obj);

void	_dumpThunk	(FILE* file, Obj* obj);
void	_dumpSusp	(FILE* file, Obj* obj);
void	_dumpData	(FILE* file, Obj* obj);
void	_dumpDataR	(FILE* file, Obj* obj);
void	_dumpDataRS	(FILE* file, Obj* obj);
void	_dumpDataM	(FILE* file, Obj* obj);

#endif
