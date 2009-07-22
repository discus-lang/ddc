// -- Error handling

#include "Error.h"
#include "Prim.h"

#include "Collect.ci"
#include "Alloc.ci"

#include <inttypes.h>


// User errors -------------------------------------------------------------------------------------
//	These will probably be the users fault

// This needs to be defined Control.Exception
Obj*	Base_ExceptionCaseNoMatch (Obj* moduleName, Obj* line, Obj* column);
Obj*	Data_String_boxString	  (String str);

// Die due to non-matching pattern
void	_deathCase (const char* moduleName, Int32 line, Int32 column)
{
	primException_throw 
		(Base_ExceptionCaseNoMatch 
			(  Data_String_boxString ((Char8*)moduleName)
			, _boxInt32	(line)
			, _boxInt32	(column)));
	abort();
}



// Internal errors ---------------------------------------------------------------------------------
//	These will probably be due to a bug in the runtime system.

// Ran out of heap space.
//	This can also be happen when we run out of heap space.
//	The implementation can't grow the heap yet.
void	_panicOutOfHeap (UInt allocCount, UInt64 heapSize)
{
	fprintf (stderr, "*** DDC RTS PANIC! Out of heap space.\n");
	fprintf (stderr, "        current (full) heap size: %" PRId64 " bytes\n", heapSize);
	fprintf (stderr, "      could not allocate another: %d bytes\n",   allocCount);
	abort();
}


// Ran out of slots on the shadow stack.
// 	This can also be due to regular stack overflow problems in the user source.
void	_panicOutOfSlots (void)
{
	fprintf ( stderr
		, "*** DDC RTS PANIC! Slot stack overflow.\n");

	_dumpPanic();
	abort();
}


// We don't have enough eval functions in Eval.ci, or Eval.ci is wrong.
void	_panicApply (void)
{
	fprintf ( stderr
		, "*** DDC RTS PANIC! Not enough apply/eval functions.\n");

	_dumpPanic();
	abort();
}


// This is likely a bug in the runtime system.
void	_panicSlotUnderflow (void)
{
	fprintf ( stderr
		, "*** DDC RTS PANIC! Slot stack underflow.\n");

	_dumpPanic();
	abort();
}

// This is likely a bug in the runtime system.
void	_panicCorruption (void)
{
	fprintf ( stderr
		, "*** DDC RTS PANIC! Heap/stack corruption.\n");
		
	_dumpPanic();
	abort();
}
