
// The Garbage Collector.

#include "Object.h"
#include "Collect.h"
#include "Prim.h"
#include "Lint.h"
#include "Macro.h"
#include "Dump.h"
#include "Error.h"
#include "Util.h"

#include "Alloc.ci"

#include <string.h>


// For debugging.
//	_DDC_TRACE_GC is set in Config.h

#if	_DDC_TRACE_GC
static inline void _TRACE (const char* format, ...)
{
	va_list	ap;

	va_start (ap, format);
	vfprintf (_traceFile, format, ap);
	va_end(ap);
}
#	define _TRACES(s)	s;

#else
#	define _TRACE(x,...)
#	define _TRACES(s)
#endif


// Allocate and initialise the current GC slot stack.
void	_collectInit (UInt maxGCSlots)
{
	_ddcSlotBase	= malloc (sizeof (Obj*) * maxGCSlots);
	_ddcSlotPtr	= _ddcSlotBase;
	_ddcSlotMax	= _ddcSlotBase + maxGCSlots - 1;

	_ddcProfile ->slot.base
			= _ddcSlotBase;
}


// Perform a collection on on this heap.
//	This is a simple Cheney-scan collection.
void	_collectHeap
		( Word8*	heapBase	// Start of the from space.
		, Word8*	heapPtr		// Where the next object would be allocated.
		, Word8*	UNUSED (heapMax) 	// Last byte of the from space.

		, Word8*	heapBackBase	// Start of the to space.
		, Word8**	heapBackPtr)	// This is set to where the next object would be allocated
						//	in the to space, after the live data has been copied across.
{
	// Debugging
	_TRACES(char	fileName[80]);
	_TRACES(snprintf (fileName, 80, "ddc-rts.trace.collect.%04lu", _gcCount));
	_TRACES(_traceFile	= fopen (fileName, "w"));

	_TRACE ("------------------------------------------------------------------------------------------------ collectHeap\n");
	_TRACE ("  heapBase     = %p\n", heapBase);
	_TRACE ("  heapPtr      = %p\n", heapPtr);
	_TRACE ("  heapMax      = %p\n", heapMax);
	_TRACE ("  heapBackBase = %p\n", heapBackBase);
	_TRACE ("  heapBackPtr  = %p\n", *heapBackPtr);

	_TRACES(_dumpState (_traceFile));

	_DEBUG (_lintHeap  (heapBase, heapPtr));
	_DEBUG (_lintSlots (_slotBase, _slotPtr, heapBase, heapPtr));

	// -------
	// Reset the toPtr
	*heapBackPtr	= heapBackBase;

	// Evacuate all the root objects to the toHeap.
	_evacuateRoots	( heapBase
			, heapPtr
			, heapBackPtr);

	// Scan
	_scanHeap	(heapBackBase, heapBackPtr);


	// Debugging
	_TRACE ("-------------------------------------------------------------------------------------- collect Heap done\n");
	_TRACES(fprintf(_traceFile, "  newSize = %u\n", (UInt)(*heapBackPtr - heapBackBase)));
	_TRACES(_dumpState (_traceFile));
	_TRACES(_dumpHeap  (_traceFile, heapBackBase, *heapBackPtr));

	_DEBUG(_lintHeap  (heapBackBase, *heapBackPtr));
	_DEBUG(_lintSlots (_slotBase, _slotPtr, heapBackBase, *heapBackPtr));

	// Update collection counter
	_PROFILE_GC (count++);

	// Restore trace file.
	_TRACES (fclose (_traceFile));
	_TRACES (_traceFile	= stdout);
}


// Evacuate all the object which are reachable from the heap slot stack
void	_evacuateRoots
		( Word8*	UNUSED (heapBase)
		, Word8*	UNUSED  (heapPtr)
		, Word8** 	toPtr )
{
	// Debugging
	_TRACE("-------------------------------------------------------------------\n");
	_TRACE("--- Evacuating Roots\n");
	_TRACE("-------------------------------------------------------------------\n");

	Obj** ptr	 = _ddcSlotBase;
	while (ptr < _ddcSlotPtr)
	{
		if (*ptr != 0)
			*ptr	= _evacuateObj  ( *ptr, toPtr );

		ptr++;
	}

}

// Evacuate a single object and update the toPtr to point
//	to the word _after_ the new copy.
Obj*	_evacuateObj
		( Obj*		obj
		, Word8**	toPtr)
{
	// Debugging
	_TRACE("--- evacuateObj\n");
	_TRACE("    obj     = %p\n", obj);
	_TRACE("    toPtr   = %p\n", *toPtr);
	_TRACES(_printObjP (_traceFile, obj));

	_DEBUG(assert(obj != 0));
	_DEBUG(assert(toPtr != 0));

	enum _ObjType
		objType		= _objType (obj);

#if _DDC_DEBUG_GC
	bool	anchored	= _objIsAnchored (obj);
	if (objType == _ObjTypeForward
		 || !anchored)
	{
		if (obj < (Obj*)_ddcHeapBase)
			_PANIC ("Obj %p to be evacuated lies before start of heap.\n", obj);

		if (obj > (Obj*)_ddcHeapPtr)
			_PANIC ("Obj %p to be evacuated lies after end of heap.\n", obj);
	}
#endif

	// If this object is a broken heart then return the forwarding pointer.
	if (objType == _ObjTypeForward) {
		_TRACE("BROKEN-HEART\n");
		_TRACE("\n\n");

		Obj*	objR	= _readBrokenHeart (obj);

#if _DDC_DEBUG_GC
		if (_objType (objR) == _ObjTypeForward)
		{
			_ERROR ("Forwarded object is also a forward.\n");
			_ERROR ("  obj  = %p\n", obj);
			_ERROR ("  objR = %p\n", objR);
		}
#endif
		_DEBUG(assert (_objType (objR) != _ObjTypeForward));

		return objR;
	}

	else {
		// If the object is anchored then leave it alone.
		//	These are allocated outside of the regular heap,
		//	and are outside our juristiction.
		if (_objIsAnchored (obj)) {
			_TRACE("ANCHORED\n");
			_TRACE("\n\n");
			return	obj;
		}

		// Otherwise, copy the object to the toHeap.
		UInt	size	= _objSize (obj);
		Obj*	newObj	= (Obj*) *toPtr;

		memcpy (newObj, obj, size);
		*toPtr += size;

		_PROFILE_GC (copyCount++);
		_PROFILE_GC (copyBytes += size);

		// Leave a broken-heart behind.
		_writeBrokenHeart (obj, newObj);

		_TRACES(_printObjP (_traceFile, obj));
		_TRACES(_printObjP (_traceFile, newObj));
		_TRACE ("\n\n");

		return newObj;
	}
}


// Scan functions ----------------------------------------------------------------------------------
//	There is one of these for each sort of object in the heap.


// Scan a thunk
static inline
void	_scanThunk
		( Obj*		obj
		, Word8**	toPtr)
{
	_TRACE("--- scanThunk %p\n", obj);
	_TRACES(_printObjP (_traceFile , obj));

	Thunk*	thunk	= (Thunk*)obj;
	for (UInt i = 0; i < thunk->args; i++)
		thunk->a[i]	= _evacuateObj (thunk->a[i], toPtr);

}


// Scan a regular data object
static inline
void	_scanData
		( Obj*		obj
		, Word8**	toPtr)
{
	_TRACE("--- scanData\n");

	Data*	data	= (Data*)obj;
	for (UInt i = 0; i < data->arity; i++)
		data->a[i]	= _evacuateObj (data->a[i], toPtr);
}


// Scan a data object with mixed pointer and non-pointer data
static inline
void	_scanDataM
		( Obj*		obj
		, Word8**	toPtr)
{
	_TRACE("--- scanDataM\n");

	// The pointers all lie at the front of the data payload.
	DataM*	data	= (DataM*)obj;
	Obj**	ptr	= (Obj**)&(data->payload);
	for (UInt i = 0; i < data->ptrCount; i++)
		ptr[i]		= _evacuateObj (ptr[i], toPtr);
}


// Scan a suspension or indirection
static inline
void	_scanSusp
		( Obj*		obj
		, Word8**	toPtr)
{
	_TRACE("--- scanSusp %p\n", obj);
	_TRACES(_printObjP (_traceFile, obj));

	SuspIndir*	susp	= (SuspIndir*)obj;
	Tag		tag	= _getObjTag (obj);

	// Object is a zero airity suspension.
	//	The function ptr for these is stored directly in the obj field
	//	and is not an object we need to evacuate.
	//
	if ( tag == _tagSusp
	  && susp ->arity == 0)
	{
		// do nothing

	}

	// Object is a regular suspension, carry on scanning.
	else if (tag == _tagSusp)
	{
		_TRACE("    SUSPENSION\n");

		susp ->obj	= _evacuateObj (susp ->obj, toPtr);
		for (UInt i = 0; i < susp ->arity; i++)
			susp->a[i]	= _evacuateObj (susp ->a[i], toPtr);
	}

	else if (tag == _tagIndir)
	{
		_TRACE("    INDIRECTION gonna evac %p\n", susp ->obj);
		susp ->obj	= _evacuateObj (susp ->obj, toPtr);
	}

	// Uh oh.
	else 	_PANIC ("_scanSusp: object is not a suspension, maybe renegade indirection?\n");

	_TRACE("--- scanSusp end %p\n", obj);
	_TRACES(_printObjP (_traceFile, obj));
	_TRACE("\n\n");
}


// Scan an object.
//	This checks the tag and calls a more specific scan function for that object.
void	_scanObj
		( Obj*		obj
		, Word8**	toPtr)
{
	_TRACE("------------------- scan %p\n", obj);
	_TRACE("    obj   = %p\n", obj);

	switch (_objType (obj)) {
	 case _ObjTypeUnknown:
	 	_PANIC ("Cannot scan unknown object.\n");
	 	break;

	 case _ObjTypeForward:
		_ERROR ("Found Broken-Heart during scan.\n");
		_ERROR ("  obj      = %p\n", 	obj);
		_ERROR ("  tagFlags = 0x%08x\n", obj ->tagFlags);
	 	_PANIC ("Cannot scan object.\n");
		break;

	 case _ObjTypeThunk:	_scanThunk (obj, toPtr);	break;
	 case _ObjTypeData:	_scanData  (obj, toPtr);	break;

	 // Raw data contains no pointers to scan.
	 case _ObjTypeDataR:	_TRACE("--- scanDataR\n");	break;

	 case _ObjTypeDataM:	_scanDataM (obj, toPtr);	break;

	 // Small, raw data contains no pointers to scan.
	 case _ObjTypeDataRS:	_TRACE("--- scanDataRS\n");	break;

	 case _ObjTypeSuspIndir: _scanSusp  (obj, toPtr);	break;

	 default:
	 	_PANIC("Object has header\n");

	}

	_TRACE("------------------- scan %p end\n", obj);
	_TRACE("\n");

}


// Scan all the objects in the to space.
//	This copies in the data that is reachable from the object already there.
void	_scanHeap
		( Word8*	heapBackBase
		, Word8**	heapBackPtr)
{
	_TRACE("-------------------------------------------------------------------\n");
	_TRACE("--- Scanning Heap\n");
	_TRACE("-------------------------------------------------------------------\n");

	Word8*	scanPtr	= heapBackBase;
	while (scanPtr < *heapBackPtr)
	{
		Obj* obj	= (Obj*)scanPtr;

		_scanObj (obj, heapBackPtr);
		scanPtr	+= _objSize (obj);
	}
}


// Forwarding Pointers -----------------------------------------------------------------------------

// On 32 bit systems:
//	When an object is moved from the 'from' space to the 'to' space,
//	its header is overwritten with a forwarding pointer (aka broken heart)
//	to its new location.
//
//	This forwarding pointer is is written over the 32 bit header of the object in
//	the from space. As heap objects are always 4 byte aligned, the lowest
//	two bits of the pointer are zero, and we use this to distinguish forwarding
//	pointers from regular objects.
//
// On 64 bit systems:
//	The header is still only 32 bits, but we now need to store a 64 bit pointer.
//	We have to put the LOW half in the header, so that the lowest two bits are
//	still zero. The HIGH half goes after this. This happens automatically on
//	little-endian systems, but on big-endian systems we have to do it manually.


#if !defined(SPLIT_POINTERS)
// 32 bit system
void	_writeBrokenHeart
		( Obj* obj
		, Obj* newObj)
{
	_DEBUG(assert(obj    != 0));
	_DEBUG(assert(newObj != 0));

	SizePtr* objPtr	= (SizePtr*)obj;
	objPtr[0]	= (SizePtr)newObj;
}

Obj*	_readBrokenHeart
		(Obj* obj)
{
	_DEBUG(assert(obj   != 0));

	SizePtr* objPtr = (SizePtr*)obj;
	Obj*	newObj	= (Obj*) objPtr[0];

	_DEBUG(assert(newObj != 0));
	return	newObj;
}

#else
// 64 bit big-endian system
void	_writeBrokenHeart
		( Obj* obj
		, Obj* newObj)
{
	_DEBUG(assert(obj    != 0));
	_DEBUG(assert(newObj != 0));

	SizePtr ptr	= (SizePtr) newObj;
	HalfPtr ptrH	= (HalfPtr) (ptr >> 32);
	HalfPtr ptrL	= (HalfPtr) (ptr & 0x0ffffffff);

	HalfPtr* objPtr	= (HalfPtr*) obj;
	objPtr[0]	= ptrL;			// LOW  half of pointer overwrites the header
	objPtr[1]	= ptrH;			// HIGH half comes after
}

Obj*	_readBrokenHeart
		(Obj* obj)
{
	_DEBUG(assert(obj != 0));

	HalfPtr* objPtr	= (HalfPtr*)obj;

	HalfPtr	ptrL	= objPtr[0];
	HalfPtr	ptrH	= objPtr[1];

	SizePtr	ptr	= ((SizePtr)ptrH << 32) | (SizePtr)ptrL;

	_DEBUG(assert(ptr != 0));
	return	(Obj*) ptr;
}

#endif
