
// These are all the macros that the DDC generated code uses

#ifndef _DDC_Macro
#define _DDC_Macro

#include "Types.h"
#include "Force.h"

// Extract the tag of an object
static inline UInt
	_TAG	(Obj* obj)	
{
	return obj ->tagFlags >> 8; 
}

// Extract an constructor argument froma data object.
#define _DARG(data,i)	(((Data*)data) ->a[i])

// Extract a functino argument from a thunk.
#define _TARG(thunk,i)	(((Thunk*)thunk) ->a[i])

// Extract a function argument from a suspension.
#define _SARG(susp,i)	(((SuspIndir*)susp) ->a[i])

// Force this object then extract a named field.
#define _FIELD(exp,type,label)	\
	(((struct type*)_force(exp))->label)

// Force this object then take a reference to a named field.
#define _FIELDR(exp,type,label)	\
	(_boxRef ( _force(exp) \
		 , &(((struct type*)_force(exp)) ->label) ) )

// Force this object.
//	The result is guarantee not to be a suspension.
#define _FORCE(v)	(_force(v))

// Follow an indirection.
#define _FOLLOW(v)	(((SuspIndir*)v) ->obj)

// These case alternatives are added to all statements that switch
//	on the tag of a data object. If the data object is a suspension
//	then it is forced or followed, then control continues from the 
//	provided label.
#define _CASESUSP(var, label) \
	case _tagSusp: \
	  var = _FORCE(var); \
	  goto label; \
	case _tagIndir: \
	  var = _FOLLOW(var); \
	  goto label; 

// Handle a non-exhaustive case match.	  
#define _CASEDEATH(file,line,col) \
	default: _deathCase (file, line, col);

// Emit a non-exaustive case match error.
#define _CASEFAIL \
	_deathCase (__func__, 0, 0);


// DDC.Store macros -------------------------------------------------------------------------------

// These are used in the source code for the DDC.Store library
#define _PEEK(ptr)		*(ptr)
#define	_PLUSPTR(ptr,offset)	(ptr + offset)


#endif

