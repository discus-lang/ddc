
// Functions to allocate new objects into the heap.

#ifndef _DDC_Alloc
#define _DDC_Alloc

#include "Object.h"

// Initialise the allocation system.
//	and allocate the heap.
void	_allocInit	(UInt 	heapSize);

// Perform a garbage collection.
void	_allocCollect	(UInt 	byteCount);


#endif
