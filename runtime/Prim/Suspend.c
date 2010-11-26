
#include "Prim.h"

#include "../Storage/Collect.h"
#include "../Storage/Collect.ci"

#include "../Storage/Alloc.h"
#include "../Storage/Alloc.ci"

// ----- Suspend
Obj*	primSuspend0 (void* f)
{
	// No need to alloc a thunk for f when it has no args.
	//	Just hack the raw fun ptr into the suspension.
	//
	SuspIndir* susp	= (SuspIndir*)_allocSusp((Obj*)f, 0);

	return (Obj*)susp;		
}


Obj*	primSuspend1 (Obj* f, Obj* x)
{
	_ENTER(1);
	_S(0)		= x;
	
	SuspIndir* susp	= (SuspIndir*)_allocSusp (f, 1);
	susp->a[0]	= _S(0);
	
	_LEAVE(1);
	return (Obj*)susp;
}	


Obj*	primSuspend2 (Obj* f, Obj* x1, Obj* x2)
{
	_ENTER(2);
	_S(0)		= x1;
	_S(1)		= x2;
	
	SuspIndir* susp	= (SuspIndir*)_allocSusp (f, 2);
	susp ->a[0]	= _S(0);
	susp ->a[1]	= _S(1);
	
	_LEAVE(2);
	return (Obj*)susp;
}


Obj*	primSuspend3 (Obj* f, Obj* x1, Obj* x2, Obj* x3)
{
	_ENTER(3);
	_S(0)		= x1;
	_S(1)		= x2;
	_S(2)		= x3;
	
	SuspIndir* susp	= (SuspIndir*)_allocSusp (f, 3);
	susp ->a[0]	= _S(0);
	susp ->a[1]	= _S(1);
	susp ->a[2]	= _S(2);
	
	_LEAVE(3);
	return (Obj*)susp;
}


Obj*	primSuspend4 (Obj* f, Obj* x1, Obj* x2, Obj* x3, Obj* x4)
{
	_ENTER(4);
	_S(0)		= x1;
	_S(1)		= x2;
	_S(2)		= x3;
	_S(3)		= x4;
	
	SuspIndir* susp	= (SuspIndir*)_allocSusp (f, 4);
	susp ->a[0]	= _S(0);
	susp ->a[1]	= _S(1);
	susp ->a[2]	= _S(2);
	susp ->a[3]	= _S(3);
	
	_LEAVE(4);
	return (Obj*)susp;
}


Obj*	primForce    (Obj* obj)
{
	return	_force (obj);
}	
