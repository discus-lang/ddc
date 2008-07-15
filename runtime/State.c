
#include "State.h"

// Global state for the runtime system.

// File to write the trace to.
FILE*		_ddcTraceFile	= 0;

// If this is true the RTS will be dumped after a panic.
bool		_ddcDumpOnPanic	= false;	

// Profiling information
_Profile*	_ddcProfile	= 0;

// A stash of the command args
Int32	_ddcArgCount		= 0;
String*	_ddcArgValue		= 0;

// The Heap ----------------------------------------------------------------------------------------

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


// GC slot stack -----------------------------------------------------------------------------------
//	Holds pointers which serve as the roots for garbage collection

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


// Contest Stack -----------------------------------------------------------------------------------
//	Holds longjmp targets for exception handling

struct Context*	_ddcContextStack	= 0;

// Index of where the NEXT context will be stored.
//	The context on the top of the stack is at (_contextIndex - 1)
int	_ddcContextIndex	= 0;	
						
// Index of the highest available context.
int	_ddcContextMax		= 0;	

// Used to pass the exception information from primExceptionThrow
//	back to primExceptionTry.
Obj*	_ddcContextObject	= 0;	

