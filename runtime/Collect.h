
// The Garbage collector.
#ifndef _DDC_Collect
#define _DDC_Collect

#include "Object.h"

// Allocate and initialise the current GC slot stack.
void	_collectInit
	(UInt maxGCSlots);


// Perform a collection on on this heap.
//	This is a simple Cheney-scan collection.
void	_collectHeap
		( Word8* 	heapBase
		, Word8* 	heapPtr
		, Word8* 	heapMax

		, Word8*	heapBackBase
		, Word8**	heapBackPtr);

// Evacuate all the object which are reachable from the heap slot stack
void	_evacuateRoots
		( Word8*	heapBase
		, Word8*	heapPtr
		, Word8** 	toPtr);

// Evacuate a single object and update the toPtr to point
//	to the word _after_ the new copy.
Obj*	_evacuateObj
		( Obj*		fromPtr
		, Word8**	toPtr);


void	_scanHeap
		( Word8*	heapBackBase
		, Word8**	heapBackPtr);

// Evacuate all the objects reachable from this one.
void	_scanObj
		( Obj*	obj
		, Word8** toPtr);

// Write a broken heart over the top of this object which
//	points to the copy of the object in the to-space.
void	_writeBrokenHeart
		( Obj*		obj
		, Obj*		newObj);

// Read a broken heart from this location.
Obj*	_readBrokenHeart
		( Obj*		obj);

#endif

