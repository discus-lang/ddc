
#include "Macro.h"
#include "Force.h"
#include "Profile.h"
#include "Collect.h"
#include "Object.h"
#include "Lint.h"
#include "State.h"

#include "Collect.ci"


// If this object is a suspended application, then force it.
Obj*	_force (Obj* obj)
{
#if _DDC_DEBUG
	_lintObjPtr (obj, _ddcHeapBase, _ddcHeapMax);
#endif
	_PROFILE_APPLY (forceCount++);

	_ENTER(1);
	_S(0)	= obj;

	// Keep forcing suspensions and following indirections
	//	until we get a real object.
	again:
	switch (_getObjTag (_S(0))) {
	 case _tagSusp:	
	 	_S(0) = _forceStep (_S(0)); 
		goto again;

	 case _tagIndir:
		_S(0) = ((Susp*)_S(0)) ->obj; 
		goto again;
	}

	Obj*	tmp	= _S(0);
	_LEAVE(1);

	return	tmp;
}


// Do one step of the forcing.
//	This does a single application.
//
Obj*	_forceStep (Obj* susp_)
{
	_DEBUG	 (assert (_TAG(susp_) == _tagSusp));
	_ENTER(1);
	_S(0)	= susp_;

	// -----
	Susp* susp	= (Susp*)_S(0);

	Obj* obj	= 0;
	switch (susp->arity) {
		case 0: _PROFILE_APPLY (force[0]++);
			Obj* (*fun)()	= (Obj* (*)()) susp->obj;
			obj	= fun ();
			break;

		case 1:	_PROFILE_APPLY (force[1]++);
			obj	= _apply1 (susp->obj, susp->a[0]);
	 	 	break;

		case 2: _PROFILE_APPLY (force[2]++); 
			obj	= _apply2 (susp->obj, susp->a[0], susp->a[1]);
	 		break;

		case 3: _PROFILE_APPLY (force[3]++);
			obj	= _apply3 (susp->obj, susp->a[0], susp->a[1], susp->a[2]);
			break;

		case 4: _PROFILE_APPLY (force[4]++);
			obj	= _apply4 (susp->obj, susp->a[0], susp->a[1], susp->a[2], susp->a[3]);
			break;

		default:
			_panicApply();
	}


	// Overwrite the suspension with an indirection to the result.
	Susp* susp2	= (Susp*)_S(0);
	susp2 ->tagFlags= (_tagIndir << 8) | _ObjFixedSusp;
	susp2 ->obj	= obj;
	

#if _DDC_DEBUG
	// Zap any remaining args for debugging purposes
	for (UInt i = 0; i < susp2 ->arity; i++)
		susp2 ->a[i]	= 0;
#endif

	_LEAVE(1);
	return obj;
}
