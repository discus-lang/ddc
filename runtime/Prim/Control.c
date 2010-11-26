
#include "../Prim.h"
#include "../Context.h"
#include "../Util.h"

#include "../Storage/Collect.h"
#include "../Storage/Collect.ci"


// ----- Control
Obj*	primControl_while
		( Obj*	test_
		, Obj*	body_)
{
	_ENTER(2);
	_S(0)	= test_;
	_S(1)	= body_;

	int ret	= _contextPush();

	// we pushed the context, so do the loop.
	if (ret == CONTEXT_RET_PUSH)
	{
		while (_getObjTag (_force (_apply1 (_S(0), _primUnit ))) == _tagTrue)
		{
			_apply1 (_S(1), _primUnit);
		}

		_LEAVE(2);
		_contextPop();
		return _primUnit;
	}

	// we caught a break.
	else if (ret == CONTEXT_RET_BREAK)
	{
		_LEAVE(2);
		_contextPop( );
		return _primUnit;
	}

	// this message isn't for us
	else {
		_LEAVE(2);
		_contextPop();
		_contextAgain(ret);
		return _primUnit;
	}


}


Obj*	primControl_break
		( Obj*	UNUSED (unit))
{
	_contextBreak();
	return _primUnit;
}


Obj*	primControl_when
		( Obj*	test_
		, Obj*	body_)
{
	_ENTER(2);
	_S(0)	= test_;
	_S(1)	= body_;

	if    (_getObjTag (_force (_apply1 (_S(0), _primUnit ))) == _tagTrue)
	{
		_apply1 (_S(1), _primUnit);
	}

	_LEAVE(2);
	return _primUnit;
}


Obj*	primControl_unless
		( Obj*	test_
		, Obj*	body_)
{
	_ENTER(2);
	_S(0)	= test_;
	_S(1)	= body_;

	if    (_getObjTag (_force (_apply1 (_S(0), _primUnit ))) == _tagFalse)
	{
		_apply1 (_S(1), _primUnit);
	}

	_LEAVE(2);
	return _primUnit;
}


