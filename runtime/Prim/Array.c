
#include <string.h>
#include "Prim.h"
#include "../Force.h"

#include "../Collect.h"
#include "../Collect.ci"

#include "../Alloc.h"
#include "../Alloc.ci"

#include <stdio.h>

// Should be defined in Base/Exception.ts
extern Obj* Base_ExceptionArrayBounds (Obj* size, Obj* index);

// -- new
Obj*	primArray_new_unsafe_noInit
		(Obj* elemCount_)
{
	// unboxing
	UInt	elemCount	= _unbox(Int32, elemCount_);

	// alloc the object
	Data*	array		= (Data*)_allocData (0, elemCount);
	
	// clear the pointers.
	memset (array ->a, 0, sizeof (Obj*) * elemCount);
	
	return (Obj*)array;
}


// -- new
Obj*	primArray_new
		(Obj* elemCount_, Obj* obj_)
{
	_ENTER(2);
	_S(0)	= elemCount_;
	_S(1)	= obj_;
	
	// unboxing
	UInt	elemCount	= _unbox(Int32, _S(0));
	
	// alloc the object
	Data*	array		= (Data*)_allocData (0, elemCount);
	
	// initialize
	for (UInt i = 0; i < elemCount; i++)
		array ->a[i] = _S(1);
		
	_LEAVE(2);
	return (Obj*)array;
}


// -- size
Obj*	primArray_size
		(Obj* array_)
{
	// unboxing
	Data*	array		= (Data*)_force	(array_);
	
	return	_boxInt32 (array ->arity);
}


// -- index
Obj*	primArray_index
		(Obj* array_, Obj* ix_)
{
	_ENTER(2);
	_S(0)	= array_;
	_S(1)	= ix_;
	
	// unboxing
	Data*	array		= (Data*)_force	(_S(0));
	UInt	ix		= _unbox(Int32, _S(1));

	// bounds check
	if (ix >= array ->arity) {
		_LEAVE(2);
		primException_throw
			(Base_ExceptionArrayBounds
				( _boxInt32 (array ->arity)
				, _boxInt32 (ix) ));
	}
	
	_LEAVE(2)
	return	array->a [ix];
}


// -- indexR
Obj*	primArray_indexR
		(Obj* array_, Obj* ix_)
{
	_ENTER(2);
	_S(0)	= array_;
	_S(1)	= ix_;
	
	// unboxing
	Data*	array		= (Data*)_force	(_S(0));
	UInt	ix		= _unbox(Int32, _S(1));
	
	// bounds check
	if (ix >= array ->arity) {
		_LEAVE(2);
		primException_throw
			(Base_ExceptionArrayBounds
				( _boxInt32 (array ->arity)
				, _boxInt32 (ix) ));
	}
	
	Obj*	ref		= _boxRef (array_, &(array ->a[ix]));
	
	_LEAVE(2);
	return ref;
}
