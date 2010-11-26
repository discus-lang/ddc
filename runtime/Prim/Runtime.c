
#include "../Runtime.h"
#include "../Storage/Alloc.ci"


Int32	primRuntime_slotUsage (Obj* UNUSED (x))
{
	return	((SizePtr)_ddcSlotPtr - (SizePtr)_ddcSlotBase) / sizeof (Obj*);
}


Obj*	primRuntime_printObj (Obj* x)
{
	_dumpObj (stdout, x);
	return _primUnit;
}
