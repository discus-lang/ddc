
#include "Lint.h"
#include "Object.h"
#include "Error.h"
#include "Macro.h"

// Debugging
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


// Trace through the heap looking for malformed objects or bad pointers.
void	_lintHeap
		( Word8* base
		, Word8* top)
		
{
	_TRACE("* lintHeap ------------------------------------------------------------------------- lintHeap\n");
	_TRACE("  base = %p\n", base);
	_TRACE("  top  = %p\n", top);

	Word8*	ptr	= base;
	while (ptr < top)
	{
		Obj*	obj	= (Obj*)ptr;
		
		enum _ObjType
			objType	= _objType (obj);
			
		// Check for forwards.
		if (objType == _ObjTypeForward) 
			_PANIC ("_lintHeap: Obj %p is a Forward can't lint a heap containing forwards.\n", obj);

		// Check known object type.
		if (objType == _ObjTypeUnknown) 
		{
			_ERROR ("_lintHeap: unknown object type\n");
			_ERROR ("  obj      = %p\n", 	obj);
			_ERROR ("  tagFlags = 0x%02x\n", obj ->tagFlags);
			_PANIC ("_lintHeap: Heap is broken.\n");
		}

		switch (objType) {
		 case _ObjTypeThunk:	_lintThunk (obj, base, top);	break;
		 case _ObjTypeSusp: 	_lintSusp  (obj, base, top);	break;
		 case _ObjTypeData:	_lintData  (obj, base, top);	break;
		 case _ObjTypeDataM:	_lintDataM (obj, base, top);	break;
		 case _ObjTypeDataR:	break;
		 case _ObjTypeDataRS:	break;
		 
		 default:		_PANIC ("Bad object type\n");
		 
		}

		ptr	+= _objSize (obj);
	}

	_TRACE ("  Heap looks OK --------------------------------------------------------------------\n");
	_TRACE ("\n\n");
}


// Lint all the data reachable from the GC slot stack.
void	_lintSlotsD()
{
	_lintSlots 
		( _ddcSlotBase
		, _ddcSlotPtr
		, _ddcHeapBase
		, _ddcHeapPtr);
}

void	_lintSlots
		( Obj** 	slotBase
		, Obj**  	slotPtr
		, Word8* 	heapBase
		, Word8*	heapTop)
{
	_TRACE("lintSlots ------------------------------------------------------------------- lintSlots\n");
	_TRACE("  slotBase = %p\n", slotBase);
	_TRACE("  slotPtr  = %p\n", slotPtr);
	_TRACE("  s0       = %p\n", *slotPtr);
	
	Obj**	ptr	= slotBase;
	while (ptr < slotPtr)
	{
		if (*ptr != 0) 
			_lintObjPtr (*ptr, heapBase, heapTop);

		ptr++;
	}

	_TRACE ("  Slots look OK --------------------------------------------------------------------\n");
	_TRACE ("\n\n");
}


// Lint a pointer which is supposed to point to a valid object.
void	_lintObjPtr
		( Obj*		obj
		, Word8*	base
		, Word8*	top)
{
	enum _ObjType	
		objType		= _objType (obj);

	if (!_objIsAnchored (obj))
	{
		if (obj < (Obj*)base)
		{
			_ERROR ("Object lies before start of heap.\n");
			_ERROR ("  obj        = %p\n", obj);
			_ERROR ("  heapBase   = %p\n", base);
			_ERROR ("  heapTop    = %p\n", top);
			_panicCorruption();
		}

		if (obj > (Obj*)top)
		{
			_ERROR ("Object starts after end of heap.\n");
			_ERROR ("  obj        = %p\n", obj);
			_ERROR ("  heapBase   = %p\n", base);
			_ERROR ("  heapTop    = %p\n", top);
			_panicCorruption();
		}

		if ((Word8*)obj + _objSize (obj) > (Word8*)top) 
		{
			_ERROR ("Object spills off the end of the heap.\n");
			_ERROR ("  obj        = %p\n", obj);
			_ERROR ("  size       = %d\n", _objSize(obj));
			_ERROR ("  obj + size = %p\n", (Word8*)obj + _objSize(obj));
			_ERROR ("  heapBase   = %p\n", base);
			_ERROR ("  heapTop    = %p\n", top);
			_panicCorruption();
		}
	}

	if (objType == _ObjTypeUnknown)
	{
		_ERROR ("Object type is Unknown.\n");
		_ERROR ("  obj        = %p\n", obj);
		_ERROR ("  tagFlags   = 0x%08x", obj ->tagFlags);
		_panicCorruption();
	}

	if (objType == _ObjTypeForward)
	{
		_ERROR ("Object is a Forward.\n");
		_ERROR ("  obj        = %p\n", obj);
		_panicCorruption();
	}
}


// Lint an Thunk object which has a valid header.
void	_lintThunk
		( Obj*		obj
		, Word8*	base
		, Word8*	top)
{
	Thunk* thunk	= (Thunk*) obj;

	for (UInt i = 0; i < thunk ->args; i++)
		_lintObjPtr (thunk ->a[i], base, top);
}


// Lint a Susp object which has a valid header.
void	_lintSusp
		( Obj* 		obj
		, Word8*	base
		, Word8*	top)
{
	_lintObjPtr (obj, base, top);

	Susp* susp	= (Susp*) obj;
	
	if (_getObjTag(obj) == _tagIndir)
	{
		_lintObjPtr (susp ->obj, base, top);
	}

	else if (_getObjTag(obj) == _tagSusp)
	{
		_lintObjPtr (susp ->obj, base, top);
		
		for (UInt i = 0; i < susp->arity; i++)
			_lintObjPtr (susp ->a[i], base, top);
	}
	
	else 	_PANIC ("Bad tag for Susp\n");
}


// Lint a Data object which has a valid header.
void	_lintData
		( Obj* 		obj
		, Word8*	base
		, Word8*	top)
{
	Data* data	= (Data*) obj;
	
	for (UInt i = 0; i < data ->arity; i++)
		_lintObjPtr (data ->a[i], base, top);
}


// Lint a mixed data object which has a vlaid header.
void	_lintDataM
		( Obj*		obj
		, Word8*	base
		, Word8*	top)
{
	DataM* data	= (DataM*) obj;
	
	Obj**	ptr	= (Obj**)(&data ->payload);
	
	for (UInt i = 0; i < data ->ptrCount; i++)
		_lintObjPtr (ptr[i], base, top);
}

