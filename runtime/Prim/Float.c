
#include "Prim.h"

#include "../Collect.h"
#include "../Collect.ci"

#include "../Alloc.h"
#include "../Alloc.ci"

#include <math.h>

#define MAKE_PRIMOP1FUN(Type, op, opName) \
	Obj* prim##Type##_##opName (Obj* a) \
	{	return _box##Type (op _unbox##Type(a)); }

#define MAKE_PRIMOP2FUN(Type, op, opName) \
	Obj* prim##Type##_##opName (Obj* a, Obj* b) \
	{	return _box##Type (_unbox##Type(a) op _unbox##Type(b)); } 

#define MAKE_PRIMCOMPFUN(Type, op, opName) \
	Obj* prim##Type##_##opName (Obj* a, Obj* b) \
	{	if (_unbox##Type(a) op _unbox##Type(b)) \
			return _primTrue; \
		else	return _primFalse; \
	}


// -----
// -- Primitive operations
//
MAKE_PRIMOP1FUN  (Float32, -, neg);

MAKE_PRIMOP2FUN  (Float32, +, add);
MAKE_PRIMOP2FUN  (Float32, -, sub);
MAKE_PRIMOP2FUN  (Float32, *, mul);
MAKE_PRIMOP2FUN  (Float32, /, div);

MAKE_PRIMCOMPFUN (Float32, ==, eq);
MAKE_PRIMCOMPFUN (Float32, !=, neq);
MAKE_PRIMCOMPFUN (Float32, >,  gt);
MAKE_PRIMCOMPFUN (Float32, >=, ge);
MAKE_PRIMCOMPFUN (Float32, <,  lt);
MAKE_PRIMCOMPFUN (Float32, <=, le);



Obj*	primFloat32_mod (Obj* x1_, Obj* x2_)
{
	_ENTER(2);
	_S(0)	= x1_;
	_S(1)	= x2_;
	
	float x1	= _unbox(Float32, x1_);
	float x2	= _unbox(Float32, x2_);
	
	_LEAVE(2);
	return	_boxFloat32 (fmodf (x1, x2));
}

Obj*	primFloat32_toFloat (Obj* x1)
{
	Int32 x		= _unbox(Int32, x1);
	return	_boxFloat32 (x);
}

Obj*	primFloat32_truncate (Obj* x_)
{
	float x		= _unbox(Float32, x_);
	return	_boxInt32 (x);
}


Obj*	primFloat32_update (Obj* dest_, Obj* src_)
{
	_ENTER(1);
	_S(0)	= dest_;
	
	float src	= _unbox(Float32, src_);
	DataRS*  data	= (DataRS*) _force(dest_);
	Float32* xp	= (Float32*) data ->payload;
	*xp		= src;
	
	_LEAVE(1);
	return _primUnit;
}



