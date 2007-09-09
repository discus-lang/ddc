
#ifndef _DDC_Lint
#define _DDC_Lint

#include "Types.h"


void	_lintHeap 
		( Word8*	base
		, Word8*	top);

void	_lintSlotsD();

void	_lintSlots
		( Obj**		slotBase
		, Obj**		slotPtr
		, Word8*	heapBase
		, Word8*	heapTop);



void	_lintThunk	(Obj* obj, Word8* base, Word8* top);
void	_lintData	(Obj* obj, Word8* base, Word8* top);
void	_lintSusp	(Obj* obj, Word8* base, Word8* top);
void	_lintData	(Obj* obj, Word8* base, Word8* top);
void	_lintDataM	(Obj* obj, Word8* base, Word8* top);
		
void	_lintObjPtr
		( Obj*		obj
		, Word8*	base
		, Word8*	top);


#endif

