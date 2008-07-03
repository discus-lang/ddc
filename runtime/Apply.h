
#ifndef _Trauma_Apply
#define _Trauma_Apply

#include "Types.h"

// Sets what apply functions we have available.
//	After changing these you must also edit EvalGen.hs and run
//	makeEval.sh to actually generate the eval functions.
//

// The arity of the longest callable function.
//	This places a hard limit on the functions that can be written in the user code.
#define	_evalMaxAirity	20	

// The maximum number of immediate args to functions. 
//	this is equal to the the number of _applyN functions we have.
#define _evalMaxImm	4	
				
Obj*	_apply1		(Obj*	thunk, Obj* x1);
Obj*	_apply2		(Obj*	thunk, Obj* x1, Obj* x2);
Obj*	_apply3		(Obj*	thunk, Obj* x1, Obj* x2, Obj* x3);
Obj*	_apply4		(Obj*	thunk, Obj* x1, Obj* x2, Obj* x3, Obj* x4);


#endif

