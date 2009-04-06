
#ifndef _DDC_Macro
#define _DDC_Macro

#include "Types.h"
#include "Force.h"

// Macros used in DDC.Store
#define _PEEK(ptr)		*(ptr)
#define	_PLUSPTR(ptr,offset)	(ptr + offset)


// Hackery on prim structures
static inline UInt
	_TAG	(Obj* obj)	
{
	return obj ->tagFlags >> 8; 
}

#define _DARG(data,i)	(((Data*)data) ->a[i])
#define _TARG(thunk,i)	(((Thunk*)thunk) ->a[i])
#define _SARG(susp,i)	(((Susp*)susp) ->a[i])

#define _FIELD(exp,type,label)	(((struct type*)_force(exp))->label)
#define _FIELDR(exp,type,label)	\
	(_boxRef ( _force(exp) \
		 , &(((struct type*)_force(exp)) ->label) ) )

// Hackery on suspensions
#define _FORCE(v)	(_force(v))
#define _FOLLOW(v)	(((Susp*)v) ->obj)


// Extra case alternatives.
#define _CASESUSP(var, label) \
	case _tagSusp: \
	  var = _FORCE(var); \
	  goto label; \
	case _tagIndir: \
	  var = _FOLLOW(var); \
	  goto label; 

// Force loop hackery
#define _FORCELOOP(dest,src) \
	  
#define _CASEDEATH \
	default: _deathCase (__func__, 0, 0);

#define _CASEFAIL \
	_deathCase (__func__, 0, 0);

// Data tags
#define _null		0
#define _tagThunk	0x0ffffff
#define _tagSusp	0x0fffffe
#define _tagIndir	0x0fffffd

#define _tagBase	0

#define _tagFalse       (_tagBase + 0)
#define _tagTrue        (_tagBase + 1)



#endif

