
#include <stdint.h>
#include <inttypes.h>

#include "Prim.h"

#include "../Collect.h"
#include "../Collect.ci"

#include "../Alloc.h"
#include "../Alloc.ci"

// We can't just return primTrue / primFalse here
//	because the return region might be mutable.
//
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
MAKE_PRIMOP1FUN  (Int64, -, neg);

MAKE_PRIMOP2FUN  (Int64, +, add);
MAKE_PRIMOP2FUN  (Int64, -, sub);
MAKE_PRIMOP2FUN  (Int64, *, mul);
MAKE_PRIMOP2FUN	 (Int64, /, div);
MAKE_PRIMOP2FUN  (Int64, %, mod);


MAKE_PRIMCOMPFUN (Int64, ==, eq);
MAKE_PRIMCOMPFUN (Int64, !=, neq);
MAKE_PRIMCOMPFUN (Int64, >,  gt);
MAKE_PRIMCOMPFUN (Int64, >=, ge);
MAKE_PRIMCOMPFUN (Int64, <,  lt);
MAKE_PRIMCOMPFUN (Int64, <=, le);


// -----
Obj* primInt64_update	(Obj* dest_, Obj* src_)
{
	_DEBUG	(assert ( _getObjTag(dest_) == _tagBase));
	_DEBUG	(assert ( _getObjTag(src_)  == _tagBase));

	// -----
	_ENTER(2);
	_S(0)	= dest_;
	_S(1)	= src_;

	_S(0)	= _force(_S(0));
	_S(1)	= _force(_S(1));


	DataRS* dest	= (DataRS*)_S(0);
	Int64*  destX	= (Int64*)dest->payload;

	DataRS* src	= (DataRS*)_S(1);
	Int64* srcX	= (Int64*)src->payload;

	*destX		= *srcX;

	_LEAVE(2);
	return _primUnit;
}

void	primInt64_to_string	(Int64 value, char * str, int slen)
{
	// Need to do it this way to correctly print an Int64, because
	// we need "%lld" on 32 bit systems and "%ld" on 64 bit systems.
	snprintf (str, slen, "%" PRId64, value);
}

