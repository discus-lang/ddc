
#ifndef _Trauma_Apply
#define _Trauma_Apply

#include "Types.h"

// ----
// Eval
//	After changing these you must also edit EvalGen.hs and run
//	makeEval.sh to actually generate the eval functions.
//
#define	_evalMaxAirity	20	// The airity of the longest callable function.
#define _evalMaxImm	4	// The maximum number of immediate args to functions. 
				//	-- this == the number of applyN functions we have.
				
Obj*	_apply1		(Obj*	thunk, Obj* x1);
Obj*	_apply2		(Obj*	thunk, Obj* x1, Obj* x2);
Obj*	_apply3		(Obj*	thunk, Obj* x1, Obj* x2, Obj* x3);
Obj*	_apply4		(Obj*	thunk, Obj* x1, Obj* x2, Obj* x3, Obj* x4);



#endif

