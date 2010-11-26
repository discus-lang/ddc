
#include "Runtime.h"
#include <stdlib.h>

// _contextSetup
//	Sets up the context stack ready for a call to setjmp()
//
//	We can't actually call setjmp here, because when a function that calls setjmp
//	returns, its context is invalidated. (from the man-page for setjmp).
//
//	Clients which actually want to push a context onto the stack should use the 
//	inlined _contextPush() function.
//
jmp_buf* _contextSetup ()
{
	// Check to see if we have a free context on the stack.
	if (_ddcContextIndex >= _ddcContextMax) 
		_PANIC ("Context stack overflow.");

	_PROFILE_CONTEXT (setupCount++);
	
	// Setup our record on the stack.
	struct Context* 
		context		= &(_ddcContextStack [_ddcContextIndex++]);

	context ->slotPtr	= _ddcSlotPtr;

#if 	_DDC_PROFILE_CONTEXT
	if (_ddcContextIndex > _ddcProfile ->context.highWater)
		_ddcProfile ->context.highWater = _ddcContextIndex;
#endif

	return &(context ->env);
}


// _contextRestore
//	Restore the context that lies on top of the context stack.
//	The context remains on the stack. 
//	It's the restored functions job to pop the context when its finished with it.
//	
void	_contextRestore (int retValue)
{
	if (_ddcContextIndex <= 0)
		_PANIC ("Context stack underflow.");

	_PROFILE_CONTEXT (restoreCount++);

	// Get the context from the top of the stack.
	struct Context* context
		= &(_ddcContextStack [_ddcContextIndex-1]);
		
	// Restore the context.
	_ddcSlotPtr	= context ->slotPtr;
	longjmp (context ->env, retValue);
	
	// Longjump should have restored the context
	//	We should never make it here.
	_PANIC ("Failed to restore context.");
}





