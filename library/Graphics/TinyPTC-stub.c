
#include "runtime/Runtime.h"
#include "TinyPTC-X11-0.7.3/tinyptc.h"

Obj*	ptcOpen (Obj* name_, Obj* sizeX_, Obj* sizeY_)
{
	_ENTER(3);
	Char8* 	name	= _unbox (String, name_);
	UInt	sizeX	= _unbox (Int32,  sizeX_);
	UInt	sizeY	= _unbox (Int32,  sizeY_);

	int	ret	= ptc_open (name, sizeX, sizeY);

	assert (ret == PTC_SUCCESS);

	_LEAVE(3);
	return	_primUnit;
}


Obj*	ptcClose (Obj* unit)
{
	ptc_close();
	return	_primUnit;
}


Obj*	ptcUpdate (Obj* array_)
{
	ptc_update (primArrayU_Int_getBufPtr (array_) );
	return	_primUnit;
}

