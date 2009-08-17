// Global state for the runtime system
//	All the global state variables are defined in this file.
//
#include "State.h"


// Profiling information
//	We maintain a global pointer to the profiling data structure,
//	which is updated as the program progresses.
//
_Profile*	_ddcProfile	= 0;

// A stash of the command args
Int32	_ddcArgCount		= 0;
String*	_ddcArgValue		= 0;


// The Heap -------------------------------------------------------------------
//	_ddcHeap*      is the current heap in use (the from-space)
//	_ddcHeapBack*  is the where we copy live objects during garbage
//		       collection (the to-space)
//
//	On each garbage collection we copy all live objects from the
//	"from" to the "two" space, then swap the pointers.
//

// The first object is allocated at this addr.
Word8*	_ddcHeapBase		= 0;	
	
// The next object is allocated starting from this addr.
Word8*	_ddcHeapPtr		= 0;	
						
// Points to the last byte in the heap which can be allocated.
Word8*	_ddcHeapMax		= 0;
			
// The heap (to space)			
Word8*	_ddcHeapBackBase	= 0;	
Word8*	_ddcHeapBackPtr		= 0;
Word8*	_ddcHeapBackMax		= 0;


// GC slot stack --------------------------------------------------------------
//	The GC slot stack holds the set of GC roots, that is, pointers
//	to the objects currently being used by the program. All objects
//	must be reachable from the root set across any function that might
//	trigger a GC.
	
// Base address of slot stack.
Obj**	_ddcSlotBase		= 0;	

// The top of the slot stack.
//	The next slot is allocated here.
//	_S(0) is *(slotPtr-1)
Obj**	_ddcSlotPtr		= 0;	

// The maximum value that _slotPtr can take without overflowing the stack.
Obj**	_ddcSlotMax		= 0;	

// The highest _ddcSlotPtr we've seen so far (used during profiling)
Obj**	_ddcSlotHighWater	= 0;


// Context Stack --------------------------------------------------------------
// 	The Context stack holds evaluation contexts created by entering a
//	"try" construct. If the body of the "try" throws an exception then
//	we resume execution from the context that's on the top of the stack.
//	All this is done with the POSIX longjmp mechanism.
//
struct Context*	_ddcContextStack	= 0;

// Index of where the NEXT context will be stored.
//	The context on the top of the stack is at (_contextIndex - 1)
int	_ddcContextIndex	= 0;	
						
// Index of the highest available context.
int	_ddcContextMax		= 0;	

// Used to pass the exception information from primExceptionThrow
//	back to primExceptionTry.
Obj*	_ddcContextObject	= 0;	


// Debugging / Tracing state --------------------------------------------------
//	State for debugging the runtime system.

// File to write the trace to.
FILE*		_ddcTraceFile	= 0;

// If this is true then the heap will be dumped after a panic.
bool		_ddcDumpOnPanic	= false;	

