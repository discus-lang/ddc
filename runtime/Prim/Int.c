
#include "Prim.h"

#include "../Collect.h"
#include "../Collect.ci"

#include "../Alloc.h"
#include "../Alloc.ci"

// We can't just return primTrue / primFalse here
//	because the return region might be mutable.
//	


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
MAKE_PRIMOP2FUN  (Int32, +, add);
MAKE_PRIMOP2FUN  (Int32, -, sub);
MAKE_PRIMOP2FUN  (Int32, *, mul);
MAKE_PRIMOP2FUN	 (Int32, /, div);
MAKE_PRIMOP2FUN  (Int32, %, mod);


MAKE_PRIMCOMPFUN (Int32, ==, eq);
MAKE_PRIMCOMPFUN (Int32, !=, neq);
MAKE_PRIMCOMPFUN (Int32, >,  gt);
MAKE_PRIMCOMPFUN (Int32, >=, ge);
MAKE_PRIMCOMPFUN (Int32, <,  lt);
MAKE_PRIMCOMPFUN (Int32, <=, le);


// -----
Obj* primInt32_update	(Obj* dest_, Obj* src_)
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
	Int32*  destX	= (Int32*)dest->payload;

	DataRS* src	= (DataRS*)_S(1);
	Int32* srcX	= (Int32*)src->payload;
	
	*destX		= *srcX;

	_LEAVE(2);
	return _primUnit;
}	
