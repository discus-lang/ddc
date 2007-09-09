
#include "Dump.h"
#include "State.h"
#include "Object.h"


void	_dumpPanic ()
{
	if (!_ddcDumpOnPanic)
		return;

	FILE* file	= fopen ("ddc.dump", "w");
	
	_dumpState (file);
	_dumpHeap  (file, _ddcHeapBase, _ddcHeapPtr);

	fclose (file);
}


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
		_printObjP (file, obj);
		
		ptr	+= _objSize (obj);
	}
}
