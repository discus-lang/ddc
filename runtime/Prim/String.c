
#include "Prim.h"
#include "../Runtime.h"
#include "../Apply.h"
#include "../Object.h"

#include "../Collect.h"
#include "../Collect.ci"

#include "../Alloc.h"
#include "../Alloc.ci"

#include <string.h>

Obj*	primString_eq (Obj* str1_, Obj* str2_)
{
	_ENTER(2);
	_S(0)	= str1_;
	_S(1)	= str2_;
	
	Word8* cStr1	= _unbox(String, _S(0));
	Word8* cStr2	= _unbox(String, _S(1));
	
	Int32 isEq	= strcmp(cStr1, cStr2);

	printf ("\ncompare %p %p '%s' '%s' %d\n", _S(0), _S(1), cStr1, cStr2, isEq);
	
	_LEAVE(2);
	if (isEq == 0)
		return _primTrue;
	else	return _primFalse;
}


Obj*	primString_heads (Obj* str_)
{
	Word8* cStr	= _unbox(String, str_);
	
	Word8 s[2];
	s[0]	= cStr[0];
	s[1]	= 0;
	
	return _boxString (s);
}


Obj*	primString_tails (Obj* str_)
{
	Word8* cStr	= _unbox(String, str_);
	assert (cStr[0] != 0);
	
	return	_boxString (cStr + 1);
}


Obj*	primString_isNul (Obj* str_)
{
	Word8*	cStr	= _unbox(String, str_);
	
	if (cStr[0] == 0)
		return _primTrue;
	else	return _primFalse;
}


Obj*	primString_ord	(Obj* str_)
{
	Word8*	cStr	= _unbox(String, str_);
	
	return	_boxInt32 (cStr[0]);
}
