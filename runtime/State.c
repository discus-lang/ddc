
#include "State.h"

// -- Config.
FILE*		_ddcTraceFile	= 0;
bool		_ddcDumpOnPanic	= false;	// If this is true the state is dumped after a panic.

// -- Profile.
_Profile*	_ddcProfile		= 0;


// --- The heap
Word8*		_ddcHeapBase		= 0;	// Base address of heap.	
						//	First object in heap is allocated starting from this addr.
	
Word8*		_ddcHeapPtr		= 0;	// Heap alloc pointer.
						//	Next object in heap is allocated starting from this addr.

Word8*		_ddcHeapMax		= 0;	// Top of heap.
						//	Points to the last byte in the heap which can be allocated.


Word8*		_ddcHeapBackBase	= 0;	
Word8*		_ddcHeapBackPtr		= 0;
Word8*		_ddcHeapBackMax		= 0;


// -- Slot stack.
Obj**		_ddcSlotBase		= 0;	// Base address of slot stack.
Obj**		_ddcSlotPtr		= 0;	// The top of the slot stack.
						//	The next slot is allocated here.
						//	_S(0) is *(slotPtr-1)

Obj**		_ddcSlotMax		= 0;	// The maximum value that _slotPtr can take without overflowing the stack.
Obj**		_ddcSlotHighWater	= 0;


// -- Context stack.
struct Context*	_ddcContextStack	= 0;
int		_ddcContextIndex	= 0;	// Index of where the NEXT context will be stored.
						//	The context on the top of the stack is at (_contextIndex - 1)

int		_ddcContextMax		= 0;	// Index of the highest available context.
Obj*		_ddcContextObject	= 0;	// Used to pass the exception information from primExceptionThrow
						//	back to primExceptionTry.





