
#include "Prim.h"

#include "../Collect.h"
#include "../Collect.ci"

#include "../Alloc.h"
#include "../Alloc.ci"

Int32	primRuntime_slotUsage (Obj* x)
{
	return	((SizePtr)_ddcSlotPtr - (SizePtr)_ddcSlotBase) / sizeof (Obj*);
}


Obj*	primRuntime_printObj (Obj* x)
{
	_printObj (stdout, x);
	return _primUnit;
}
