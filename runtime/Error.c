// -- Error handling

#include "Error.h"
#include "Prim.h"

#include "Collect.ci"
#include "Alloc.ci"

// -- Errors which probably the user's fault --------------------------------------------
// This needs to be defined Control.Exception
Obj*	Control_Exception_ExceptionCaseNoMatch (Obj* moduleName, Obj* line, Obj* column);

// Die due to non-matching pattern
void	_deathCase (const char* moduleName, Int32 line, Int32 column)
{
	primException_throw 
		(Control_Exception_ExceptionCaseNoMatch 
			( _boxString 	((Char8*)moduleName)
			, _boxInt32	(line)
			, _boxInt32	(column)));
	abort();
}

// -- Errors which are probably due to a bug in the runtime system ----------------------

// -- Ran out of heap space.
void	_panicOutOfHeap (UInt allocCount, UInt64 heapSize)
{
	fprintf (stderr, "*** DDC RTS PANIC! Out of heap space.\n");
	fprintf (stderr, "        current (full) heap size: %lld bytes\n", heapSize);
	fprintf (stderr, "      could not allocate another: %d bytes\n",   allocCount);
	abort();
}


// -- Ran out of slots on the shadow stack.
// -- 	This can also be due to regular stack overflow problems in the user source.
void	_panicOutOfSlots (void)
{
	fprintf ( stderr
		, "*** DDC RTS PANIC! Slot stack overflow.\n");

	_dumpPanic();
	abort();
}


// -- We don't have enough eval functions in Eval.ci, or Eval.ci is wrong.
void	_panicApply (void)
{
	fprintf ( stderr
		, "*** DDC RTS PANIC! Not enough apply/eval functions.\n");

	_dumpPanic();
	abort();
}


// -- This is likely a bug in the runtime system.
void	_panicSlotUnderflow (void)
{
	fprintf ( stderr
		, "*** DDC RTS PANIC! Slot stack underflow.\n");

	_dumpPanic();
	abort();
}

// -- This is likely a bug in the runtime system.
void	_panicCorruption (void)
{
	fprintf ( stderr
		, "*** DDC RTS PANIC! Heap/stack corruption.\n");
		
	_dumpPanic();
	abort();
}
