
#ifndef _Trauma_Prim
#define _Trauma_Prim

#include "../Object.h"

// -----
// -- These will be defined by the actual user module that exports them
// --
extern Obj*	True();
extern Obj*	False();
extern Obj*	Unit();

// -----------------------------------------------------------------------------
// -- Globals
// -----------------------------------------------------------------------------

// defined in Prim.c
extern Obj*	_primUnit;		
extern Obj*	_primTrue;		
extern Obj*	_primFalse;		


// -----------------------------------------------------------------------------
// -- Boxing.c
// -----------------------------------------------------------------------------
Obj*	_boxRef				(Obj*  obj, void* field);

Obj*	primFloat32_toFloat 		(Obj* x1);
Obj*	primFloat32_truncate		(Obj* x1);
Obj*	primFloat64_toFloat 		(Obj* x1);
Obj*	primFloat64_truncate		(Obj* x1);
void	primInt64_toString		(Int64 value, char * str, int slen);
void	primWord64_toString		(Word64 value, char * str, int slen);

// -- Suspend
Obj*	primSuspend0			(void* f);
Obj*	primSuspend1			(Obj* f, Obj* x);
Obj*	primSuspend2			(Obj* f, Obj* x1, Obj* x2);
Obj*	primSuspend3			(Obj* f, Obj* x1, Obj* x2, Obj *x3);
Obj*	primSuspend4			(Obj* f, Obj* x1, Obj* x2, Obj *x3, Obj *x4);

// -- Control
Obj*	primControl_while		(Obj* test, Obj* body);
Obj*	primControl_break		(Obj* unit);
Obj*	primControl_when		(Obj* test, Obj* body);
Obj*	primControl_unless		(Obj* test, Obj* body);

// -- Exceptions
Obj*	primException_try 		(Obj* function, Obj* handler);
Obj*	primException_throw	 	(Obj* exception);
Obj*	primException_init 		(Obj* contextStackDepth);
Obj*	primException_handleUncaught	(Obj* exception);

// -- Array
Obj*	primArray_new			(Obj* elemCount_, Obj* obj);
Obj*	primArray_size			(Obj* array_);
Obj*	primArray_index			(Obj* array_, Obj* ix_);
Obj*	primArray_indexR		(Obj* array_, Obj* ix_);

// -- ArrayU
Obj*	primArrayU_Int_new 		(Obj* anchored, Obj* elemCount_);
Obj*	primArrayU_Int_get 		(Obj* array_, Obj* ix_);
Int32*	primArrayU_Int_getBufPtr	(Obj* array_);
Obj*	primArrayU_Int_set		(Obj* array_, Obj* ix_, Obj* x_);
Obj*	primArrayU_Int_dump 		(Obj* array_);
Obj*	primArrayU_Int_size		(Obj* array_);
Obj*	primArrayU_Int_fill		(Obj* array_, Obj* x_);

Obj*	primError			(Obj* str)	__attribute__ ((noreturn));
Obj*	primExit			(Obj* str)	__attribute__ ((noreturn));

Obj*	primRefUpdate			(Obj* ref, Obj* x);
Int32	primConnect			(String hostname, Int32 port);
Int32	primArgCount			(Obj* x);
String	primArgValue			(Int32 x);

// -- Runtime
Int32	primRuntime_slotUsage		(Obj* x);
Obj*	primRuntime_printObj		(Obj* x);

// -----
#include "Boxing.ci"
#include "ArrayU.ci"
#include "Store.ci"

#endif


