
#include "Collect.h"
#include "Prim.h"
#include "Object.h"
#include "Lint.h"
#include "Macro.h"
#include "Dump.h"
#include "Error.h"

#include <string.h>


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


// -- Collect
void	_collectHeap 
		( Word8*	heapBase
		, Word8*	heapPtr
		, Word8*	heapMax 

		, Word8*	heapBackBase
		, Word8**	heapBackPtr)
{
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

	// -----
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


void	_evacuateRoots 
		( Word8*	heapBase
		, Word8*	heapPtr
		, Word8** 	toPtr )
{
	_TRACE("-------------------------------------------------------------------\n");
	_TRACE("--- Evacuating Roots\n");
	_TRACE("-------------------------------------------------------------------\n");

	// Evacuate all the objects pointed to by the slot stack.
	Obj** ptr	 = _ddcSlotBase;
	while (ptr < _ddcSlotPtr)
	{
		if (*ptr != 0) 
			*ptr	= _evacuateObj  ( *ptr, toPtr );

		ptr++;
	}

}


Obj*	_evacuateObj
		( Obj*		obj
		, Word8**	toPtr)
{
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
		if (obj < (Obj*)_heapBase) 
			_PANIC ("Obj %p to be evacuated lies before start of heap.\n", obj);

		if (obj > (Obj*)_heapPtr)
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
		

// --------------------
// -- Scan
// --

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


	_TRACE("--- scanThunk end %p\n", obj);
}


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


static inline
void	_scanDataM
		( Obj*		obj
		, Word8**	toPtr)
{
	_TRACE("--- scanDataM\n");
	
	DataM*	data	= (DataM*)obj;
	Obj**	ptr	= (Obj**)&(data->payload);
	for (UInt i = 0; i < data->ptrCount; i++)
		ptr[i]		= _evacuateObj (ptr[i], toPtr);
}


static inline
void	_scanSusp
		( Obj*		obj
		, Word8**	toPtr)
{
	_TRACE("--- scanSusp %p\n", obj);
	_TRACES(_printObjP (_traceFile, obj));
	
	Susp*	susp	= (Susp*)obj;
	Tag	tag	= _getObjTag (obj);

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

	 case _ObjTypeDataR:	_TRACE("--- scanDataR\n");	break;
	 case _ObjTypeDataM:	_scanDataM (obj, toPtr);	break;
	 case _ObjTypeDataRS:	_TRACE("--- scanDataRS\n");	break;
	 
	 case _ObjTypeSusp:	_scanSusp  (obj, toPtr);	break;

	 default:
	 	_PANIC("no match\n");
	 
	}

	_TRACE("------------------- scan %p end\n", obj);
	_TRACE("\n");
	
}


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
