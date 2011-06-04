// Functions for pointer mangling from disciple code.

#include "Runtime.h"

Int32		_ddcPointerSize		= sizeof (void*);

// Return the address of an Obj struct.
void *
_ddcObjAddress (Obj * obj)
{
	return (void *) obj;
}

// Calculate the difference between two pointer addresses in bytes.
Int32
_ddcPointerDiff (void * a, void * b)
{
	return ((char *) a) - ((char *) b);
}
