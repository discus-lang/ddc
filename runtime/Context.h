
// Contexts record the current state of the runtime system on entering a try block.
//	When the program enters a try block we push the current context onto the
//	stack. When it throws an exception we resume execution from the context
//	on the top of the stack.
//
#ifndef _DDC_Context
#define _DDC_Context

#include "Object.h"
#include "Runtime.h"
#include "Prim.h"
#include "State.h"
#include <setjmp.h>

// Magic numbers used by the function that throws an exception to tell
//	the exception handler what type of exception it was.
#define CONTEXT_RET_PUSH	0
#define CONTEXT_RET_THROW	1
#define CONTEXT_RET_BREAK	2


// Holds an excecution context that we can resume.
struct Context
{
	// The C stack context.
	//	This is used by longjump to restore the C stack
	//	to a previous state.
	jmp_buf env;
	
	// The GC slot ptr
	//	When we restore the execution context we also restore the 
	//	slot stack to its previous state.
	Obj**	slotPtr;
};


// Set up the context stack ready for a call to setjmp()
jmp_buf* 	_contextSetup	();

// Restore the context that lies on top of the context stack.
void		_contextRestore	(int retValue);

// Push the current execution context onto the stack.
//	Note that this is a macro so we don't push an execution
//	context that's currently inside another function call.
#define _contextPush() \
	setjmp (*_contextSetup());

// Pop a context off the context stack.
static inline void _contextPop ()
	{ _ddcContextIndex--; }

// Restore the context on the top of the stack, 
//	and say that we're throwing an exception.
static inline void _contextThrow ()
	{ _contextRestore (CONTEXT_RET_THROW); }

// Restore the context on the top of the stack, 
//	and say that we're breaking out of a loop.
static inline void _contextBreak ()
	{ _contextRestore (CONTEXT_RET_BREAK); }

// Restore the context on the top of the stack,
//	and tell the exception handler this exception code.
static inline void _contextAgain (int ret)
	{ _contextRestore (ret); }


#include "Context.ci"

#endif
