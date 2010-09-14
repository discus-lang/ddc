// -- Error handling

#include <stddef.h>
#include <inttypes.h>
#include <string.h>

#include "Error.h"
#include "Prim.h"

#include "Collect.ci"
#include "Alloc.ci"


// User errors -------------------------------------------------------------------------------------
//	These will probably be the users fault

// This needs to be defined Control.Exception
Obj*	Base_ExceptionCaseNoMatch (Obj* moduleName, Obj* line, Obj* column);

// Die due to non-matching pattern
void	_deathCase (const char* moduleName, Int32 line, Int32 column)
{
	_ENTER(1);

	// TODO: This is a hacky construction of a FlatString to hold the module name.
	//       When we can put unboxed values in algebraic data we'll want to pass
	//       The string pointer directly.
	_S(0)		= _allocDataR (0, 8 + 256);
	DataR* strObj	= (DataR*)_S(0);
 	uint32_t* buf32	= (uint32_t*) ( &(strObj ->payload) );
	buf32[0]	= 256;
	buf32[1]	= strlen(moduleName);
	char* str	= (char*)(buf32 + 2);
	strncpy (str, moduleName, 256);

	primException_throw
		(Base_ExceptionCaseNoMatch
			( _S(0)
			, _boxInt32	(line)
			, _boxInt32	(column)));
			
	_LEAVE(1);
	abort();
}



// Internal errors ---------------------------------------------------------------------------------
//	These will probably be due to a bug in the runtime system.

// Ran out of heap space.
//	This can also be happen when we run out of heap space.
//	The implementation can't grow the heap yet.
void	_panicOutOfHeap (size_t allocCount, size_t heapSize)
{
	fprintf (stderr, "*** DDC RTS PANIC! Out of heap space.\n");
	fprintf (stderr, "        current (full) heap size: %" PRId64 " bytes\n", heapSize);
	fprintf (stderr, "      could not allocate another: %zu bytes\n",   allocCount);
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
