
#ifndef _DDC_Lint
#define _DDC_Lint

#include "Object.h"


// Trace through the heap looking for malformed objects or bad pointers.
//	Panics and stops the program if it finds any problems.
void	_lintHeap 
		( Word8*	base	// Base of heap.
		, Word8*	top);	// The word _after_ the last one in the last allocated object.


// Lint all the data reachable from the current GC roots.
void	_lintHeapCurrentlyReachable();

// Lint all the data reachable from this slot stack.
void	_lintSlots
		( Obj**		slotBase
		, Obj**		slotPtr
		, Word8*	heapBase
		, Word8*	heapTop);

// Lint a pointer which is supposed to point to a valid object.
void	_lintObjPtr
		( Obj*		obj
		, Word8*	base
		, Word8*	top);

// Lint individual objects.
void	_lintThunk	(Obj* obj, Word8* base, Word8* top);
void	_lintData	(Obj* obj, Word8* base, Word8* top);
void	_lintSusp	(Obj* obj, Word8* base, Word8* top);
void	_lintData	(Obj* obj, Word8* base, Word8* top);
void	_lintDataM	(Obj* obj, Word8* base, Word8* top);
		


#endif

