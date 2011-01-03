
#include "../Runtime.h"
#include "../Storage/Alloc.ci"
#include <string.h>

// NOTE: These boxing functions are defined here for use by the runtime system
//       and external C glue code only. The compiled Disciple programs treat 
//       boxing and unboxing as primitives and expand out their own code for it.

Obj* 	_boxRef (Obj* obj_, void* field)
{
	_ENTER(2);

	// The alloc below might cause GC.
	//	Store the _offset_ of the field so we can reconstruct the field ptr after the alloc.
	_S(0)			= obj_;
	size_t offset		= field - (void*)obj_;

	// Alloc a new ref.
	//	We allocate 1 'real' ptr, which points to the start of the object and is followed by GC.
	//	as well as  1 'raw'  ptr, which points to the field to update, which is not followed by GC.
	//
	DataM* 	data		= (DataM*)_allocDataM (_tagBase, 1, sizeof (Obj*));
	void**	payload		= (void**) data ->payload;

	// set the object and field ptrs.
	payload[0]		= _S(0);
	payload[1]		= (void*)_S(0) + offset;

	_LEAVE(2);
	return	(Obj*)data;
}

