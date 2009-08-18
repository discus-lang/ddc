
// Functions to allocate new objects into the heap.

#ifndef _DDC_Alloc
#define _DDC_Alloc

#include "Object.h"

// Initialise the allocation system.
//	and allocate the heap.
void	_allocInit	(UInt 	heapSize);

// Perform a garbage collection.
void	_allocCollect	(UInt 	byteCount);

// Allocate various sorts of objects.
static inline Obj* 	_allocData	(Tag	tag, 	UInt words);
static inline Obj*	_allocDataR	(Tag	tag,	UInt dataSize);
static inline Obj*	_allocDataRS	(Tag 	tag,	UInt dataSize);
static inline Obj*	_allocDataM	(Tag	tag, 	UInt ptrCount,	UInt dataSize);
static inline Obj*	_allocThunk	(FunPtr	func,	UInt airity,	UInt args);
static inline Obj*	_allocSusp	(Obj*	thunk,	UInt args);

// Allocate anchored data, from the C/malloc heap
//	This data isn't under the GC's control.
static inline Obj*	_allocData_anchored	(Tag	tag, 	UInt words);
static inline Obj*	_allocDataR_anchored	(Tag	tag,	UInt dataSize);

// Copy a thunk.
static inline Thunk*	_copyThunk	(Thunk*	thunk);



#endif
