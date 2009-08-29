
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
MAKE_PRIMOP1FUN  (Float64, -, neg);

MAKE_PRIMOP2FUN  (Float64, +, add);
MAKE_PRIMOP2FUN  (Float64, -, sub);
MAKE_PRIMOP2FUN  (Float64, *, mul);
MAKE_PRIMOP2FUN  (Float64, /, div);

MAKE_PRIMCOMPFUN (Float64, ==, eq);
MAKE_PRIMCOMPFUN (Float64, !=, neq);
MAKE_PRIMCOMPFUN (Float64, >,  gt);
MAKE_PRIMCOMPFUN (Float64, >=, ge);
MAKE_PRIMCOMPFUN (Float64, <,  lt);
MAKE_PRIMCOMPFUN (Float64, <=, le);



Obj*	primFloat64_mod (Obj* x1_, Obj* x2_)
{
	_ENTER(2);
	_S(0)	= x1_;
	_S(1)	= x2_;

	double x1	= _unbox(Float64, x1_);
	double x2	= _unbox(Float64, x2_);

	_LEAVE(2);
	return	_boxFloat64 (fmod (x1, x2));
}

Obj*	primFloat64_toFloat (Obj* x1)
{
	Float64 x	= _unbox(Int64, x1);
	return	_boxFloat64 (x);
}

Obj*	primFloat64_truncate (Obj* x_)
{
	double x	= _unbox(Float64, x_);
	return	_boxInt64 (x);
}


Obj*	primFloat64_update (Obj* dest_, Obj* src_)
{
	_DEBUG	(assert ( _TAG(dest_) == _tagBase));
	_DEBUG	(assert ( _TAG(src_)  == _tagBase));

	// -----
	_ENTER(2);
	_S(0)	= dest_;
	_S(1)	= src_;

	_S(0)	= _force(_S(0));
	_S(1)	= _force(_S(1));

	DataRS* dest	= (DataRS*)_S(0);
	Float64* destX	= (Float64*)dest->payload;

	DataRS* src	= (DataRS*)_S(1);
	Float64* srcX	= (Float64*)src->payload;

	*destX		= *srcX;

	_LEAVE(2);
	return _primUnit;
}



