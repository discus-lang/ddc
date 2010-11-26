
#include "../Runtime.h"
#include "../Prim.h"
#include "../Context.h"
#include "../Profile.h"

#include "../Storage/Collect.h"
#include "../Storage/Collect.ci"

// primTry
//	primTry :: (() -> a) -> (Exception -> a) -> a
//
Obj*	primException_try 
		( Obj*	function_
		, Obj*	handler_)
{
	_ENTER(2);
	_S(0)	= function_;
	_S(1)	= handler_;
	
	// Hello again.
	int ret	= _contextPush ();

	// If we've stored the context, then apply the function.
	if (ret == CONTEXT_RET_PUSH) 
	{	
		_PROFILE_CONTEXT (trysEntered++);

		Obj* a	= _apply1 (_S(0), _primUnit);
		
		// Ok, the function has returned without an exception,
		//	we can pop our context off the stack and carry on.
		_contextPop ();

		_PROFILE_CONTEXT (trysContinue++);
		_LEAVE(2);

		return a;	
	}

	// If we've caught an exception, then call the handler.
	else if (ret == CONTEXT_RET_THROW)
	{
		_PROFILE_CONTEXT (trysCaught++);

		// Pop our context off the stack.
		//	If the handler throws then we'll restore the next highest context.
		_contextPop ();

		Obj* a	= _apply1 (_S(1), _ddcContextObject);
		_LEAVE(2);
		return a;
	}

	// This message isn't for us.
	else 
	{
		_PROFILE_CONTEXT (trysThrough++);

		_contextPop   ();
		_contextAgain (ret);
	}

	// Due to setjmp / longjmp we should never make it here.
	_PANIC ("primException_try: Control flow wierdness.");
}


// primThrow
//	primThrow :: Exception -> a
//
Obj*	primException_throw 
		(Obj* exception)
{
	_PROFILE_CONTEXT (throwCount++);
	
	// Stash the exception info so that primException_try will
	//	find it once we've restored its context.
	_ddcContextObject	= exception;

	// Goodbye cruel world.
	_contextThrow ();

	// Due to setjmp / longjmp we should never make it here.
	_PANIC ("primException_throw: Control flow wierdness.");
}


