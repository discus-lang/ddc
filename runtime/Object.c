
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

