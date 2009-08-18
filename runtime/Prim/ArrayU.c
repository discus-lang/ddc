
#include <string.h>
#include "Prim.h"

#include "../Force.h"

#include "../Collect.h"
#include "../Collect.ci"

#include "../Alloc.h"
#include "../Alloc.ci"

#include "ArrayU.ci"

#include <stdio.h>

// Should be defined in Base/Exception.ts
Obj*	Base_ExceptionArrayBounds (Obj* size, Obj* index);



// -- new
Obj*	primArrayU_Int_new 
		(Obj* anchored_, Obj* elemCount_)
{
	_ENTER(2);
	_S(0)		= anchored_;
	_S(1)		= elemCount_;

	// unboxing
	UInt	anchored	= _unbox(Int32, _S(0));
	UInt	elemCount	= _unbox(Int32, _S(1));

	// alloc the object
	UInt	payloadSize
			= sizeof (struct ArrayU_Int_Payload)	
			+ sizeof (Int32) * elemCount;	
	
	DataR*	data;
	if (anchored)
		data	= (DataR*)_allocDataR_anchored	(0, payloadSize);
	else	data	= (DataR*)_allocDataR		(0, payloadSize);

	struct ArrayU_Int_Payload*  payload	
			= (struct ArrayU_Int_Payload*)data ->payload;

	// zap out the elements
	memset (payload, 0, payloadSize);

	// setup
	payload ->elemCount	= elemCount;

	_LEAVE(2);
	return	(Obj*)data;
}


// -- get
Obj*	primArrayU_Int_get 
		( Obj*	array_
		, Obj*	ix_)
{
	_ENTER(2);
	_S(0)		= array_;
	_S(1)		= ix_;

	// unboxing
	DataR*	array	= (DataR*)_force (_S(0));
	UInt	ix	= _unbox(Int32, _S(1));

	struct ArrayU_Int_Payload* payload
		= (struct ArrayU_Int_Payload*)array ->payload;
			
	// -- array bounds check
	if (ix >= payload ->elemCount)
		primException_throw 
			(Base_ExceptionArrayBounds 
				( _boxInt32 (payload ->elemCount)
				, _boxInt32 (ix)));
	
	// boxing
	_LEAVE(2);
	return _boxInt32 (payload ->elem[ix]);
}


// -- getBufPtr
Int32*	primArrayU_Int_getBufPtr
		( Obj* array_ )
{
	DataR*	array		= (DataR*)_force (array_);

	struct ArrayU_Int_Payload* payload
		= (struct ArrayU_Int_Payload*)array ->payload;

	return	payload->elem;
}


// -- set
Obj*	primArrayU_Int_set 
		( Obj*	array_
		, Obj*	ix_
		, Obj*	x_)
{
	_ENTER(3);
	_S(0)	= array_;
	_S(1)	= ix_;
	_S(2)	= x_;

	// unboxing
	DataR*	array		= (DataR*)_force (_S(0));
	UInt	ix		= _unbox(Int32, _S(1));
	UInt	x		= _unbox(Int32, _S(2));

	struct ArrayU_Int_Payload* payload
		= (struct ArrayU_Int_Payload*)array ->payload;
		
	// -- array bounds check
	if (ix >= payload ->elemCount)
		primException_throw
			(Base_ExceptionArrayBounds
				( _boxInt32 (payload ->elemCount)
				, _boxInt32 (ix)));
			
	payload ->elem[ix]	= x;

	_LEAVE(3);
	return _primUnit;
}


// -- dump
Obj*	primArrayU_Int_dump 
		(Obj* array_)
{
	DataR*	array		= (DataR*)_force (array_);
	Int32*	payload		= (Int32*)(array ->payload);
	Int32*	elems		= (Int32*)(array ->payload + sizeof (Int32));

	UInt32	elemCount	= payload [0];
	
	UInt i;
	printf ("--- arrayU, size = %d\n", elemCount);
	for (i = 0; i < elemCount; i++) {
		printf (" %4d = %5d\n", i, elems [i]);
	}

	return _primUnit;
}


// -- size
Obj*	primArrayU_Int_size
		(Obj* array_)
{
	DataR*	array		= (DataR*)_force (array_);
	Int32*	payload		= (Int32*)(array ->payload);
	return	_boxInt32 (payload[0]);
}


// -- fill
Obj*	primArrayU_Int_fill
		(Obj* array_, Obj* x_)
{
	_ENTER(1);
	_S(0)	= array_;

	Int32 x	= _unbox(Int32, x_);
	DataR*	array		= (DataR*) _force (_S(0));
	Int32*	payload		= (Int32*)(array ->payload);
	
	memset (payload + 1, 0, sizeof(Int32) * payload[0]);
	
	_LEAVE(1);
	return _primUnit;	
}




