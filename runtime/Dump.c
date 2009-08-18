
#include "Dump.h"
#include "Object.h"
#include "State.h"
#include "Collect.h"
#include "Macro.h"
#include "Error.ci"


// Dump the whole RTS state to a file.
//	Then abort the program.
void	_dumpPanic ()
{
	if (!_ddcDumpOnPanic)
		return;

	FILE* file	= fopen ("ddc.dump", "w");
	
	_dumpState (file);
	_dumpHeap  (file, _ddcHeapBase, _ddcHeapPtr);

	fclose (file);
}


// Dump the RTS state, not in including the heap.
void	_dumpState (FILE* file)
{
	Obj** ptr	= _ddcSlotBase;

	fprintf (file, "----------------------------------------\n");
	fprintf (file, "-- RTS State\n");
	fprintf (file,"----------------------------------------\n");
	
	fprintf (file, "  _slotBase = %p\n", _ddcSlotBase);
	fprintf (file, "  _slotPtr  = %p\n", _ddcSlotPtr);
	fprintf (file, "  _slotMax  = %p\n", _ddcSlotMax);
	fprintf (file, "\n");
	
	
	fprintf (file, "----------------------------------------\n");
	fprintf (file, "-- Slot stack\n");
	fprintf (file, "----------------------------------------\n");

	while (ptr < _ddcSlotPtr)
	{
		fprintf (file, "    _slot %p = %p\n", ptr, *ptr);
		ptr++;
	}

	fprintf (file, "\n\n");
}
	

// Dump the heap contents to a file.
void	_dumpHeap 
		( FILE*		file
		, Word8*	heapBase
		, Word8*	heapPtr)
{
	fprintf (file, "-----------------------------------------\n");
	fprintf (file, "-- Heap\n");
	fprintf (file, "-----------------------------------------\n");

	fprintf (file, "  _heapBase = %p\n", _ddcHeapBase);
	fprintf (file, "  _heapPtr  = %p\n", _ddcHeapPtr);
	fprintf (file, "\n");

	Word8*	ptr	= heapBase;
	while (ptr < heapPtr)
	{
		Obj*	obj	= (Obj*)ptr;
		_dumpObjP (file, obj);
		
		ptr	+= _objSize (obj);
	}
}


// Dump an object to a file
void	_dumpObj	(FILE* file, Obj* obj)
{
	switch (_objType(obj)) {
	 case _ObjTypeUnknown:
	 	fprintf (file, "Unknown\n");
		return;

	 case _ObjTypeForward: {
		Obj*	objForward	= _readBrokenHeart (obj);
		fprintf (file, "Forward -> %p\n", objForward);
		return;
	 }
		
	 case _ObjTypeThunk:	_dumpThunk (file, obj);	return;
	 case _ObjTypeData:	_dumpData  (file, obj);	return;
	 case _ObjTypeDataR:	_dumpDataR (file, obj);	return;
	 case _ObjTypeDataM:	_dumpDataM (file, obj);	return;
	 case _ObjTypeSuspIndir: _dumpSusp  (file, obj);	return;
		
	 case _ObjTypeDataRS:	_dumpDataRS (file, obj);	return;
	}
	
	_PANIC ("_dumpObj: bad object type.\n");
}


// Dump an object with its pointer to a file.
void	_dumpObjP	(FILE* file, Obj* obj)
{
	fprintf (file, "@ %p\n", obj);
	_dumpObj (file, obj);
}


// Dump a thunk
void	_dumpThunk 	(FILE* file, Obj* obj)
{
	Thunk* thunk	= (Thunk*)obj;

	fprintf (file, "Thunk   { tag     = 0x%06x\n",	_getObjTag (obj));
	fprintf (file, "        , flags   = 0x%02x\n",	_getObjFlags (obj));
	fprintf (file, "        , func    = %p\n",	thunk ->func); 
	fprintf (file, "        , arity   = %d\n",	thunk ->arity);
	fprintf (file, "        , args    = %d\n",	thunk ->args); 
	
	for (UInt i = 0; i < thunk ->args; i++)
		fprintf (file, "        , a[%2d]   = %p\n", i, thunk ->a[i]);
		
	fprintf (file, "        }\n");
}


// Dupm a suspension
void	_dumpSusp	(FILE* file, Obj* obj)
{
	SuspIndir* susp	= (SuspIndir*)obj;
	
	fprintf (file, "Susp    { tag     = 0x%06x (%s)\n"
		, _getObjTag (obj)
		, (_getObjTag (obj) == _tagSusp) ? "susp" : "indir");
		
	fprintf (file, "        , obj     = %p\n", 	susp ->obj);
	fprintf (file, "        , airity  = %d\n", 	susp ->arity);
	
	for (UInt i = 0; i < susp->arity; i++)
		fprintf (file, "        , a[%2d]   = %p\n", i, susp->a[i]);
		
	fprintf (file, "        }\n");
}


// Dump data with pointers
void	_dumpData	(FILE* file, Obj* obj)
{
	Data* data	= (Data*)obj;

	fprintf (file, "Data    { tag     = 0x%06x\n", _getObjTag (obj));
	fprintf (file, "        , arity   = %d\n", 	data ->arity);

	
	for (UInt i = 0; i < data->arity; i++)
		fprintf (file, "        , a[%2d]   = %p\n", i, data->a[i]);
		
	fprintf (file, "        }\n");
}


// Dump raw data
void	_dumpDataR	(FILE* file, Obj* obj)
{
	DataR*	data	= (DataR*)obj;
	
	fprintf (file, "DataR   { tag     = 0x%06x\n", _getObjTag (obj));
	fprintf (file, "        , size    = %d\n",     data ->size);

	fprintf (file, "        }\n");
}


// Dump small raw data
void	_dumpDataRS 	(FILE* file, Obj* obj)
{
	DataRS* data	= (DataRS*)obj;	

	fprintf (file, "DataRS  { tag     = 0x%06x\n", _getObjTag (obj));
	fprintf (file, "        , payload = [");
	
	UInt	payloadSize
			= _getObjArg (obj) * 4;

	for (UInt i = 0; i < payloadSize; i++)
		fprintf (file, " %02x", data ->payload[i]);
		
	fprintf (file, " ]\n");
	
	fprintf (file, "        }\n");
}


// Dump mixed pointer / non-pointer data
void	_dumpDataM 	(FILE* file, Obj* obj)
{
	DataM*	data	= (DataM*)obj;
	
	fprintf (file, "DataM   { tag      = 0x%06x\n", _getObjTag (obj));
	fprintf (file, "        , size     = %d\n",	data ->size);
	fprintf (file, "        , ptrCount = %d\n",	data ->ptrCount);
	

	// print out obj pointers.
	Obj** payloadObj	= (Obj**) &(data ->payload);

	for (UInt i = 0; i < data ->ptrCount; i++)
		fprintf (file, "        , a[%2d]   = %p\n", i, payloadObj[i]);
		
	fprintf (file, "        }\n");
}

