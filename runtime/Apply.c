
#include "Object.h"
#include "Apply.h"
#include "Profile.h"
#include "Alloc.h"
#include "Macro.h"
#include "Error.h"
#include "Collect.h"
#include "Lint.h"

#include "Eval.ci"

#include <assert.h>
#include <stdlib.h>

#include "Collect.ci"
#include "Alloc.ci"

// Functions to apply extra arguments to a thunk / activation record.
//	If the number of extra arguments gives us enough to call supercominator
//	then do that. If not then we have to build a new thunk with all the 
//	previous args, plus the new ones.
//
//	TODO: 	When adding more args, build a bigger object instead of copying 
//		one which holds slack space.


// Apply another argument to a thunk.
Obj*	_apply1		(Obj* obj, Obj* x1)
{
	_DEBUG	(assert (_getObjTag(obj) == _tagThunk));
	_PROFILE_APPLY (apply[1]++);

	_ENTER(2);
	_S(1)		= x1;

	// -----
	_S(0)		= _force (obj);
	
	Thunk*	thunk	= (Thunk*)_S(0);
	if (thunk ->args + 1 == thunk ->arity) {
		x1	= _S(1);
		_LEAVE(2);
		
		switch (thunk ->arity) {
		 	case 1:   return _eval1_1  (thunk, x1);
			case 2:   return _eval2_1  (thunk, x1);
			case 3:   return _eval3_1  (thunk, x1);
			case 4:   return _eval4_1  (thunk, x1);
			case 5:   return _eval5_1  (thunk, x1);
			case 6:   return _eval6_1  (thunk, x1);
			case 7:   return _eval7_1  (thunk, x1);
			case 8:   return _eval8_1  (thunk, x1);
			case 9:   return _eval9_1  (thunk, x1);
			case 10:  return _eval10_1 (thunk, x1);
		 	case 11:  return _eval11_1 (thunk, x1);
			case 12:  return _eval12_1 (thunk, x1);
			case 13:  return _eval13_1 (thunk, x1);
			case 14:  return _eval14_1 (thunk, x1);
			case 15:  return _eval15_1 (thunk, x1);
			case 16:  return _eval16_1 (thunk, x1);
			case 17:  return _eval17_1 (thunk, x1);
			case 18:  return _eval18_1 (thunk, x1);
			case 19:  return _eval19_1 (thunk, x1);
			case 20:  return _eval20_1 (thunk, x1);
			default: _panicApply();
	  	}
	}


		Thunk*	thunkC	= _copyThunk ((Thunk*) _S(0));
		thunkC ->a [thunkC ->args++]	= _S(1);

		Obj* objR	= (Obj*)thunkC;

	// -----
	_LEAVE(2);
	return objR;
}


// Apply two more arguments to a thunk
Obj*	_apply2		(Obj* obj, Obj* x1, Obj* x2)
{
	_DEBUG (assert (_getObjTag(obj) == _tagThunk));
	_PROFILE_APPLY (apply[2]++);

	_ENTER(3);
	_S(1)		= x1;
	_S(2)		= x2;

	// -----
	_S(0)		= _force (obj);

	Thunk*	thunk	= (Thunk*)_S(0);
	if (thunk ->args + 2 == thunk ->arity) {
		x1	= _S(1);
		x2	= _S(2);
		_LEAVE(3);

		switch (thunk ->arity) {
			case 2:   return _eval2_2  (thunk, x1, x2);
			case 3:   return _eval3_2  (thunk, x1, x2);
			case 4:   return _eval4_2  (thunk, x1, x2);
			case 5:   return _eval5_2  (thunk, x1, x2);
			case 6:   return _eval6_2  (thunk, x1, x2);
			case 7:   return _eval7_2  (thunk, x1, x2);
			case 8:   return _eval8_2  (thunk, x1, x2);
			case 9:   return _eval9_2  (thunk, x1, x2);
			case 10:  return _eval10_2 (thunk, x1, x2);
			case 11:  return _eval11_2 (thunk, x1, x2);
			case 12:  return _eval12_2 (thunk, x1, x2);
			case 13:  return _eval13_2 (thunk, x1, x2);
			case 14:  return _eval14_2 (thunk, x1, x2);
			case 15:  return _eval15_2 (thunk, x1, x2);
			case 16:  return _eval16_2 (thunk, x1, x2);
			case 17:  return _eval17_2 (thunk, x1, x2);
			case 18:  return _eval18_2 (thunk, x1, x2);
			case 19:  return _eval19_2 (thunk, x1, x2);
			case 20:  return _eval20_2 (thunk, x1, x2);
			default: _panicApply();
	  	}
	}

	
	Obj* 	objR	= 0;
	Thunk*	thunk2	= (Thunk*)_S(0);
	if (thunk2 ->args + 1 == thunk2 ->arity) 
	{
		Obj* obj1	= _apply1 (_S(0),  _S(1));
		objR		= _apply1 (obj1,   _S(2));
	}

	else {
		Thunk*	thunkC	= _copyThunk (thunk2);
		thunkC ->a [thunkC ->args++]	= _S(1);
		thunkC ->a [thunkC ->args++]	= _S(2);

		objR		= (Obj*)thunkC;
	}

	// -----
	_LEAVE(3);
	return objR;
}



// Apply three more arguments to a thunk
Obj*	_apply3		(Obj* obj, Obj* x1, Obj* x2, Obj* x3)
{
	_DEBUG (assert (_getObjTag(obj) == _tagThunk));
	_PROFILE_APPLY (apply[3]++);

	_ENTER(4);
	_S(0)		= obj;
	_S(1)		= x1;
	_S(2)		= x2;
	_S(3)		= x3;

	// -----
	_S(0)		= _force (_S(0));

	Thunk*	thunk	= (Thunk*)_S(0);
	if (thunk ->args + 3 == thunk ->arity) {
		x1	= _S(1);
		x2	= _S(2);
		x3	= _S(3);
		_LEAVE(4);

		switch (thunk ->arity) {
			case 3:   return _eval3_3  (thunk, x1, x2, x3);
			case 4:   return _eval4_3  (thunk, x1, x2, x3);
			case 5:   return _eval5_3  (thunk, x1, x2, x3);
			case 6:   return _eval6_3  (thunk, x1, x2, x3);
			case 7:   return _eval7_3  (thunk, x1, x2, x3);
			case 8:   return _eval8_3  (thunk, x1, x2, x3);
			case 9:   return _eval9_3  (thunk, x1, x2, x3);
			case 10:  return _eval10_3 (thunk, x1, x2, x3);
			case 11:  return _eval11_3 (thunk, x1, x2, x3);
			case 12:  return _eval12_3 (thunk, x1, x2, x3);
			case 13:  return _eval13_3 (thunk, x1, x2, x3);
			case 14:  return _eval14_3 (thunk, x1, x2, x3);
			case 15:  return _eval15_3 (thunk, x1, x2, x3);
			case 16:  return _eval16_3 (thunk, x1, x2, x3);
			case 17:  return _eval17_3 (thunk, x1, x2, x3);
			case 18:  return _eval18_3 (thunk, x1, x2, x3);
			case 19:  return _eval19_3 (thunk, x1, x2, x3);
			case 20:  return _eval20_3 (thunk, x1, x2, x3);
			default: _panicApply();
	  	}
	}
	
	Obj*	objR	= 0;
	Thunk*	thunk2	= (Thunk*)_S(0);
	if (thunk2 ->args + 2 == thunk2 ->arity)
	{
		Obj* obj2	= _apply2 (_S(0),  _S(1), _S(2));
		objR		= _apply1 (obj2,   _S(3));
	}

	else if (thunk2 ->args + 1 == thunk2 ->arity)
	{
		Obj* obj1	= _apply1 (_S(0),  _S(1));
		objR		= _apply2 (obj1,   _S(2), _S(3));
	}

	else {
		Thunk*	thunkC	= _copyThunk (thunk2);
		thunkC ->a [thunkC ->args++]	= _S(1);
		thunkC ->a [thunkC ->args++]	= _S(2);
		thunkC ->a [thunkC ->args++]	= _S(3);

		objR		= (Obj*)thunkC;
	}

	// -----
	_LEAVE(4);
	return	objR;
}


// Apply four more arguments to a thunk.
Obj*	_apply4		(Obj* obj, Obj* x1, Obj* x2, Obj* x3, Obj* x4)
{
	_DEBUG (assert (_getObjTag(obj) == _tagThunk));
	_PROFILE_APPLY (apply[4]++);

	_ENTER(5);
	_S(0)		= obj;
	_S(1)		= x1;
	_S(2)		= x2;
	_S(3)		= x3;
	_S(4)		= x4;
	
	// -----
	_S(0)	= _force (_S(0));

	Thunk*	thunk	= (Thunk*)_S(0);
	if (thunk ->args + 4 == thunk ->arity) {
		x1	= _S(1);
		x2	= _S(2);
		x3	= _S(3);
		x4	= _S(4);
		_LEAVE(5);

		switch (thunk ->arity) {
			case 4:   return _eval4_4  (thunk, x1, x2, x3, x4);
			case 5:   return _eval5_4  (thunk, x1, x2, x3, x4);
			case 6:   return _eval6_4  (thunk, x1, x2, x3, x4);
			case 7:   return _eval7_4  (thunk, x1, x2, x3, x4);
			case 8:   return _eval8_4  (thunk, x1, x2, x3, x4);
			case 9:   return _eval9_4  (thunk, x1, x2, x3, x4);
			case 10:  return _eval10_4 (thunk, x1, x2, x3, x4);
			case 11:  return _eval11_4 (thunk, x1, x2, x3, x4);
			case 12:  return _eval12_4 (thunk, x1, x2, x3, x4);
			case 13:  return _eval13_4 (thunk, x1, x2, x3, x4);
			case 14:  return _eval14_4 (thunk, x1, x2, x3, x4);
			case 15:  return _eval15_4 (thunk, x1, x2, x3, x4);
			case 16:  return _eval16_4 (thunk, x1, x2, x3, x4);
			case 17:  return _eval17_4 (thunk, x1, x2, x3, x4);
			case 18:  return _eval18_4 (thunk, x1, x2, x3, x4);
			case 19:  return _eval19_4 (thunk, x1, x2, x3, x4);
			case 20:  return _eval20_4 (thunk, x1, x2, x3, x4);
			default: _panicApply();
	  	}
	}

	
	Obj* 	objR	= 0;
	Thunk*	thunk2	= (Thunk*)_S(0);
	if (thunk2 ->args + 3 == thunk2 ->arity)
	{
		Obj* obj3	= _apply3 (_S(0), _S(1), _S(2), _S(3));
		objR		= _apply1 (obj3,  _S(4));
	}
		
	else if (thunk2 ->args + 2 == thunk2 ->arity)
	{
		Obj* obj2	= _apply2 (_S(0), _S(1), _S(2));
		objR		= _apply2 (obj2,  _S(3), _S(4));
	}
		
	else if (thunk2 ->args + 1 == thunk2 ->arity)
	{
		Obj* obj1	= _apply1 (_S(0), _S(1));
		objR		= _apply3 (obj1,  _S(2), _S(3), _S(4));
	}

	else {
		Thunk*	thunkC	= _copyThunk (thunk2);
		thunkC ->a [thunkC ->args++]	= _S(1);
		thunkC ->a [thunkC ->args++]	= _S(2);
		thunkC ->a [thunkC ->args++]	= _S(3);
		thunkC ->a [thunkC ->args++]	= _S(4);

		objR		= (Obj*)thunkC;
	}

	// -----
	_LEAVE(5);
	return objR;
}
