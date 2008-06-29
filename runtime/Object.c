
#include "Object.h"
#include "Error.h"
#include "Collect.h"

#include "Macro.h"

enum _ObjType
	_objType (Obj* obj)
{
	Word32	id	= _getObjId (obj);
	Word32	mode	= id & _MaskObjMode;
	
	switch (mode) {
	 case _ObjModeForward:	return _ObjTypeForward;
	 case _ObjModeFixed:	return _objTypeFixed (obj);
	 case _ObjModeDataRS:	return _ObjTypeDataRS;
	 default:		return _ObjTypeUnknown;
	}

//	PANIC ("_objType: unknown object mode 0x%02x\n", mode);
}


enum _ObjType
	_objTypeFixed (Obj* obj)
{
	Word32	id	= _getObjId (obj);
	Word32	mode	= id & _MaskObjFixed;
	
	switch (mode) {
	 case _ObjFixedThunk:	return _ObjTypeThunk;
	 case _ObjFixedData:	return _ObjTypeData;
	 case _ObjFixedDataR:	return _ObjTypeDataR;
	 case _ObjFixedDataM:	return _ObjTypeDataM;
	 case _ObjFixedSusp:	return _ObjTypeSusp;
	 case _ObjFixedMapped:	return _ObjTypeMapped;
	 default:		return _ObjTypeUnknown;
	}

//	PANIC ("_objTypeFixed: unknown object mode 0x%02x\n", mode);
}


UInt	_objSize (Obj* obj)
{
	switch (_objType(obj)) {
	 case _ObjTypeForward:	return sizeof (Forward);
	 case _ObjTypeThunk:	return sizeof (Thunk) + sizeof(Obj*) * ((Thunk*)obj) ->arity;
	 case _ObjTypeData:	return sizeof (Data)  + sizeof(Obj*) * ((Data*) obj) ->arity;
	 case _ObjTypeDataR:	return ((DataR*)obj) ->size;
	 case _ObjTypeDataRS:	return sizeof (DataRS) + _getObjArg(obj) * 4;
	 case _ObjTypeDataM:	return ((DataM*)obj) ->size;
	 case _ObjTypeSusp:	return sizeof (Susp) + sizeof(Obj*) * ((Susp*)obj) ->arity;
	 default:		break;
	}

	_ERROR ("Unknown object type.\n");
	_ERROR ("  obj      = %p\n",	obj);
	_ERROR ("  tagFlags = 0x%08x\n", 	obj ->tagFlags);
	_PANIC ("Can't take the size of an unknown object.\n");
}


bool	_objIsAnchored (Obj* obj)
{
	return	(_getObjFlags (obj) & _ObjFlagAnchored) ? true : false;
}

#if !defined(SPLIT_POINTERS)
void	_writeBrokenHeart 
		( Obj* obj
		, Obj* newObj)
{
	_DEBUG(assert(obj    != 0));
	_DEBUG(assert(newObj != 0));

	SizePtr* objPtr	= (SizePtr*)obj;
	objPtr[0]	= (SizePtr)newObj;
}

Obj*	_readBrokenHeart
		(Obj* obj)
{
	_DEBUG(assert(obj   != 0));

	SizePtr* objPtr = (SizePtr*)obj;
	Obj*	newObj	= (Obj*) objPtr[0];
	
	_DEBUG(assert(newObj != 0));
	return	newObj;
}

#else
void	_writeBrokenHeart
		( Obj* obj
		, Obj* newObj)
{
	_DEBUG(assert(obj    != 0));
	_DEBUG(assert(newObj != 0));

	SizePtr ptr	= (SizePtr) newObj;
	HalfPtr ptrH	= (HalfPtr) (ptr >> 32);
	HalfPtr ptrL	= (HalfPtr) (ptr & 0x0ffffffff);

	HalfPtr* objPtr	= (HalfPtr*) obj;
	objPtr[0]	= ptrL;
	objPtr[1]	= ptrH;
}

Obj*	_readBrokenHeart 
		(Obj* obj)
{
	_DEBUG(assert(obj != 0));

	HalfPtr* objPtr	= (HalfPtr*)obj;
	
	HalfPtr	ptrL	= objPtr[0];
	HalfPtr	ptrH	= objPtr[1];
	
	SizePtr	ptr	= ((SizePtr)ptrH << 32) | (SizePtr)ptrL;

	_DEBUG(assert(ptr != 0));
	return	(Obj*) ptr;
}

#endif


// -----------------------------------------------------------------------------
// -- Object Print
// -----------------------------------------------------------------------------

void	_printObj	(FILE* file, Obj* obj)
{
	switch (_objType(obj)) {
	 case _ObjTypeUnknown:
	 	fprintf (file, "Unknown\n");
		return;

	 case _ObjTypeForward: {
		Obj*	objForward	= _readBrokenHeart (obj);
		fprintf (file, "Forward -> %p\n", objForward);
		return;
	 }
		
	 case _ObjTypeThunk:	_printThunk (file, obj);	return;
	 case _ObjTypeData:	_printData  (file, obj);	return;
	 case _ObjTypeDataR:	_printDataR (file, obj);	return;
	 case _ObjTypeDataM:	_printDataM (file, obj);	return;
	 case _ObjTypeSusp:	_printSusp  (file, obj);	return;

	 case _ObjTypeMapped:	
	 	fprintf (file, "Mapped\n");
		return;
		
	 case _ObjTypeDataRS:	_printDataRS (file, obj);	return;
	}
	
	_PANIC ("_printObj: bad object type.\n");
}


void	_printObjP	(FILE* file, Obj* obj)
{
	fprintf (file, "@ %p\n", obj);
	_printObj (file, obj);
}


// -----
void	_printThunk 	(FILE* file, Obj* obj)
{
	Thunk* thunk	= (Thunk*)obj;

	fprintf (file, "Thunk   { tag     = 0x%06x\n",	_getObjTag (obj));
	fprintf (file, "        , flags   = 0x%02x\n",	_getObjFlags (obj));
	fprintf (file, "        , func    = %p\n",	thunk ->func); 
	fprintf (file, "        , arity   = %d\n",	thunk ->arity);
	fprintf (file, "        , args    = %d\n",	thunk ->args); 
	
	for (UInt i = 0; i < thunk ->args; i++)
		fprintf (file, "        , a[%2d]   = %p\n", i, thunk ->a[i]);
		
	fprintf (file, "        }\n");
}


// -----
void	_printSusp	(FILE* file, Obj* obj)
{
	Susp* susp	= (Susp*)obj;
	
	fprintf (file, "Susp    { tag     = 0x%06x (%s)\n"
		, _getObjTag (obj)
		, (_getObjTag (obj) == _tagSusp) ? "susp" : "indir");
		
	fprintf (file, "        , obj     = %p\n", 	susp ->obj);
	fprintf (file, "        , airity  = %d\n", 	susp ->arity);
	
	for (UInt i = 0; i < susp->arity; i++)
		fprintf (file, "        , a[%2d]   = %p\n", i, susp->a[i]);
		
	fprintf (file, "        }\n");
}


// -----
void	_printData	(FILE* file, Obj* obj)
{
	Data* data	= (Data*)obj;

	fprintf (file, "Data    { tag     = 0x%06x\n", _getObjTag (obj));
	fprintf (file, "        , arity   = %d\n", 	data ->arity);

	
	for (UInt i = 0; i < data->arity; i++)
		fprintf (file, "        , a[%2d]   = %p\n", i, data->a[i]);
		
	fprintf (file, "        }\n");
}


// -----
void	_printDataR	(FILE* file, Obj* obj)
{
	DataR*	data	= (DataR*)obj;
	
	fprintf (file, "DataR   { tag     = 0x%06x\n", _getObjTag (obj));
	fprintf (file, "        , size    = %d\n",     data ->size);

	fprintf (file, "        }\n");
}


// -----
void	_printDataRS 	(FILE* file, Obj* obj)
{
	DataRS* data	= (DataRS*)obj;	

	fprintf (file, "DataRS  { tag     = 0x%06x\n", _getObjTag (obj));
	fprintf (file, "        , payload = [");
	
	UInt	payloadSize
			= _getObjArg (obj) * 4;

	for (UInt i = 0; i < payloadSize; i++)
		fprintf (file, " %02x", data ->payload[i]);
		
	fprintf (file, " ]\n");
	
	fprintf (file, "        }\n");
}


void	_printDataM 	(FILE* file, Obj* obj)
{
	DataM*	data	= (DataM*)obj;
	
	fprintf (file, "DataM   { tag      = 0x%06x\n", _getObjTag (obj));
	fprintf (file, "        , size     = %d\n",	data ->size);
	fprintf (file, "        , ptrCount = %d\n",	data ->ptrCount);
	

	// print out obj pointers.
	Obj** payloadObj	= (Obj**) &(data ->payload);

	for (UInt i = 0; i < data ->ptrCount; i++)
		fprintf (file, "        , a[%2d]   = %p\n", i, payloadObj[i]);
		
	fprintf (file, "        }\n");
}




