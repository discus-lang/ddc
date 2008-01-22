
#include "Error.h"
#include "Prim.h"

#include "Collect.ci"
#include "Alloc.ci"

// This needs to be defined Control.Exception
//
Obj*	Control_Exception_ExceptionCaseNoMatch (Obj* moduleName, Obj* line, Obj* column);

void	_deathCase (const Word8* moduleName, Int32 line, Int32 column)
{
	fprintf ( stderr
		, "*** DDC RTS ERROR: %s:%d:%d: Exception: Non-exhaustive patterns in case.\n"
		, moduleName
		, line
		, column );

	primException_throw 
		(Control_Exception_ExceptionCaseNoMatch 
			( _boxString 	((Word8*)moduleName)
			, _boxInt32	(line)
			, _boxInt32	(column)));

	_dumpPanic();
	abort();
}


void	_panicOutOfHeap (UInt allocCount, UInt64 heapSize)
{
	fprintf (stderr, "*** DDC RTS PANIC! Out of heap space.\n");
	fprintf (stderr, "        current (full) heap size: %lld bytes\n",	heapSize);
	fprintf (stderr, "      could not allocate another: %d bytes\n",	allocCount);
	abort();
}


void	_panicOutOfSlots (void)
{
	fprintf ( stderr
		, "*** DDC RTS PANIC! Slot stack overflow.\n");

	_dumpPanic();
	abort();
}


void	_panicApply (void)
{
	fprintf ( stderr
		, "*** DDC RTS PANIC! Not enough apply/eval functions.\n");

	_dumpPanic();
	abort();
}


void	_panicSlotUnderflow (void)
{
	fprintf ( stderr
		, "*** DDC RTS PANIC! Slot stack underflow.\n");

	_dumpPanic();
	abort();
}


void	_panicCorruption (void)
{
	fprintf ( stderr
		, "*** DDC RTS PANIC! Heap/stack corruption.\n");
		
	_dumpPanic();
	abort();
}
