
#include "Prim.h"
#include "../Util.h"

#include "../Storage/Collect.h"
#include "../Storage/Collect.ci"

#include "../Storage/Alloc.h"
#include "../Storage/Alloc.ci"

#include "../Dump.h"

Int32	primRuntime_slotUsage (Obj* UNUSED (x))
{
	return	((SizePtr)_ddcSlotPtr - (SizePtr)_ddcSlotBase) / sizeof (Obj*);
}


Obj*	primRuntime_printObj (Obj* x)
{
	_dumpObj (stdout, x);
	return _primUnit;
}
