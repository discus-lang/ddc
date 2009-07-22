
#include "Prim.h"
#include "../Util.h"

#include "../Collect.h"
#include "../Collect.ci"

#include "../Alloc.h"
#include "../Alloc.ci"

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
