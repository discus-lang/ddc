
#ifndef _Trauma_Prim
#define _Trauma_Prim

#include "../Types.h"

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

Obj*	_boxString	(Word8 *s);
Obj*	_boxRef		(Obj*  obj, void* field);

String	_unboxString	(Obj*	obj);



// -----------------------------------------------------------------------------

// -- Int
Obj*	primInt32_add			(Obj* x1, Obj* x2);
Obj*	primInt32_sub			(Obj* x1, Obj* x2);
Obj*	primInt32_mul			(Obj* x1, Obj* x2);
Obj*	primInt32_div			(Obj* x1, Obj* x2);
Obj*	primInt32_mod			(Obj* x1, Obj* x2);

Obj*	primInt32_eq			(Obj* a, Obj* b);
Obj*	primInt32_neq			(Obj* a, Obj* b);
Obj*	primInt32_gt			(Obj* a, Obj* b);
Obj*	primInt32_ge			(Obj* a, Obj* b);
Obj*	primInt32_lt			(Obj* a, Obj* b);
Obj*	primInt32_le			(Obj* a, Obj* b);	

Obj* 	primInt32_update		(Obj* dest_, Obj* src_);


// -- Float
Obj*	primFloat32_add			(Obj* x1, Obj* x2);
Obj*	primFloat32_sub			(Obj* x1, Obj* x2);
Obj*	primFloat32_mul			(Obj* x1, Obj* x2);
Obj*	primFloat32_div			(Obj* x1, Obj* x2);
Obj*	primFloat32_mod			(Obj* x1, Obj* x2);

Obj*	primFloat32_eq			(Obj* x1, Obj* x2);
Obj*	primFloat32_neq			(Obj* x1, Obj* x2);
Obj*	primFloat32_gt			(Obj* x1, Obj* x2);
Obj*	primFloat32_ge			(Obj* x1, Obj* x2);
Obj*	primFloat32_lt			(Obj* x1, Obj* x2);
Obj*	primFloat32_le			(Obj* x1, Obj* x2);

Obj*	primFloat32_toFloat 		(Obj* x1);

Obj*	primFloat32_truncate		(Obj* x1);
Obj*	primFloat32_update 		(Obj* dest_, Obj* src_);

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

// -- File
Obj*	primFile_openF  		(Obj* name, Obj* flags);
Obj*	primFile_openFM 		(Obj* name, Obj* flags, Obj* mode);

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


Obj*	primPrintString			(Obj* s);

// -- String
Obj*	primString_eq 			(Obj* str1_, Obj* str2_);
Obj*	primString_heads 		(Obj* str_);
Obj*	primString_tails 		(Obj* str_);
Obj*	primString_isNul 		(Obj* str_);
Obj*	primString_ord			(Obj* str_);



// -----
Obj*	primStringChar			(Obj* c);
Obj*	primStringInt			(Obj* i);
Obj*	primStringFloat32		(Obj* x);

Obj*	primError			(Obj* str)	__attribute__ ((noreturn));
Obj*	primExit			(Obj* str)	__attribute__ ((noreturn));

Obj*	primRefUpdate			(Obj* ref, Obj* x);


// -- Runtime
Int32	primRuntime_slotUsage		(Obj* x);


// -----
#include "Boxing.ci"
#include "ArrayU.ci"

#endif


