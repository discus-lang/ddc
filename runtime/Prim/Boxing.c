
#include "Prim.h"
#include "../Macro.h"
#include "../Profile.h"
#include "../Force.h"

#include "../Collect.h"
#include "../Collect.ci"

#include "../Alloc.h"
#include "../Alloc.ci"

#include <string.h>


// -----
Obj*	_boxString (String str)
{
	UInt	len	= strlen (str) + 1;

	if (len < 8)
		len	= 8;
		
	if ((len % 4) != 0)
		len	+= 4 - (len % 4);

	DataR* data	= (DataR*)_allocDataR (_tagBase, len);
	String  x	= (String)data ->payload;
	
	strcpy (x, str);
	
	_PROFILE_BOXING (bString.count++);
	_PROFILE_BOXING (bString.bytes += sizeof(DataR) + len);
	
	return (Obj*)data;
}


String	_unboxString (Obj* obj)
{
	DataR* data	= (DataR*) _force (obj);

	// make sure this is an SChunk and not an SAppend.
	assert (_TAG((Obj*)data) == 0);

	String  x	= (String) data ->payload;
	
	_PROFILE_BOXING (bString.gets++);
	return x;
}


Obj* 	_boxRef (Obj* obj_, void* field)
{
	_ENTER(2);

	// The alloc below might cause GC. 
	//	Store the _offset_ of the field so we can reconstruct the field ptr after the alloc.
	_S(0)			= obj_;
	UInt offset		= field - (void*)obj_;
	 
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

