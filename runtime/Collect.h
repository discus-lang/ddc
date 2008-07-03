
#ifndef _DDC_Collect
#define _DDC_Collect

#include "Types.h"

void	_collectInit (UInt maxGCSlots);

void	_collectHeap 	
		( Word8* 	heapBase
		, Word8* 	heapPtr
		, Word8* 	heapMax

		, Word8*	heapBackBase
		, Word8**	heapBackPtr);


void	_evacuateRoots	
		( Word8*	heapBase
		, Word8*	heapPtr
		, Word8** 	toPtr);

Obj*	_evacuateObj	
		( Obj*		fromPtr
		, Word8**	toPtr);			

void	_scanHeap 
		( Word8*	heapBackBase
		, Word8**	heapBackPtr);

void	_scanObj
		( Obj*	obj
		, Word8** toPtr);

void	_writeBrokenHeart
		( Obj*		obj
		, Obj*		newObj);

Obj*	_readBrokenHeart
		( Obj*		obj);

#endif

