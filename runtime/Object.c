
// Utils for determining the layout of objects in the heap.

#include <stddef.h>
#include <string.h>
#include <ctype.h>

#include "Object.h"
#include "Error.h"
#include "Collect.h"

#include "Macro.h"


// Determine the type of this object.
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


// Determine the type of an object, knowing that it has a fixed format field layout.
//	(ie, it's not a forwarding pointer or a DataRS)
enum _ObjType
	_objTypeFixed (Obj* obj)
{
	Word32	id	= _getObjId (obj);
	Word32	mode	= id & _MaskObjFixed;

	switch (mode) {
	 case _ObjFixedThunk:		return _ObjTypeThunk;
	 case _ObjFixedData:		return _ObjTypeData;
	 case _ObjFixedDataR:		return _ObjTypeDataR;
	 case _ObjFixedDataM:		return _ObjTypeDataM;
	 case _ObjFixedSuspIndir:	return _ObjTypeSuspIndir;
	 default:			return _ObjTypeUnknown;
	}
}


// Determine the total size of this object.
UInt	_objSize (Obj* obj)
{
	switch (_objType(obj)) {
	 case _ObjTypeThunk:
		return sizeof (Thunk) + sizeof(Obj*) * ((Thunk*)obj) ->arity;

	 case _ObjTypeData:
		return sizeof (Data)  + sizeof(Obj*) * ((Data*) obj) ->arity;

	 case _ObjTypeDataR:
		return ((DataR*)obj) ->size;

	 case _ObjTypeDataRS:
		return sizeof (DataRS) + _getObjArg(obj) * 4;

	 case _ObjTypeDataM:
		return ((DataM*)obj) ->size;

	 case _ObjTypeSuspIndir:
		return sizeof (SuspIndir) + sizeof(Obj*) * ((SuspIndir*)obj) ->arity;

	 default:			break;
	}

	_ERROR ("Unknown object type.\n");
	_ERROR ("  obj      = %p\n",	obj);
	_ERROR ("  tagFlags = 0x%08x\n", 	obj ->tagFlags);
	_PANIC ("Can't take the size of an unknown object.\n");
}

// Determine whether the object is anchored and cannot
//	be moved by the garbage collector.
bool	_objIsAnchored (Obj* obj)
{
	return	(_getObjFlags (obj) & _ObjFlagAnchored) ? true : false;
}

//------------------------------------------------------------------------------
// Functions for debugging the runtime.

static void
hexdump (unsigned char * ptr, int len)
{
	for (int k = 0 ; k < len ;  k++)
		printf ("%02x ", ptr [k]) ;
}

static void
chardump (unsigned char * ptr, int len)
{
	putchar ('"') ;
	for (int k = 0 ; k < len ;  k++)
		putchar (isprint (ptr [k]) ? ptr [k] : '.') ;
	putchar ('"') ;
}

static void
innerDumpObj (int level, Obj* obj)
{
	char indent [2 * level + 1] ;
	int id ;

	memset (indent, ' ', sizeof (indent)) ;
	indent [sizeof (indent) - 1] = 0 ;

	printf ("%sObj %p (%d bytes)   tag 0x%x\n", indent, obj, _objSize (obj), _getObjTag (obj)) ;

	id = _getObjId (obj) ;

	switch (id & 0xF7) {
	 case 0x11 :
		printf ("%s    id    : 0x%x (Thunk)\n", indent, id) ;
		printf ("%s    func  : %p\n", indent, ((Thunk*)obj)->func) ;
		printf ("%s    arity : %d\n", indent, ((Thunk*)obj)->arity) ;
		printf ("%s    args  : %d\n", indent, ((Thunk*)obj)->args) ;
		for (int k = 0 ; k < ((Thunk*)obj)->args ;  k++)
			innerDumpObj (level + 1, ((Thunk*)obj)->a [k]) ;
		break ;

	 case 0x21 :
		printf ("%s    id    : 0x%x (Data)\n", indent, id) ;
		printf ("%s    arity : %d\n", indent, ((Data*)obj)->arity) ;
		for (int k = 0 ; k < ((Data*)obj)->arity ;  k++)
			innerDumpObj (level + 1, ((Data*)obj)->a [k]) ;
		break ;

	 case 0x31 :
		printf ("%s    id      : 0x%x (DataR)\n", indent, id) ;
		printf ("%s    size    : %d\n", indent, ((DataR*)obj)->size) ;
		printf ("%s    payload : ", indent) ;
		hexdump (((DataR*)obj)->payload, ((DataR*)obj)->size - offsetof (DataR, payload)) ;
		printf ("\n%s            : ", indent) ;
		chardump (((DataR*)obj)->payload, ((DataR*)obj)->size - offsetof (DataR, payload)) ;
		puts ("") ;
		break ;

	 case 0x41 :
		printf ("%s    id       : 0x%x (DataM)\n", indent, id) ;
		printf ("%s    size     : %d\n", indent, ((DataM*)obj)->size) ;
		printf ("%s    ptrCount : %d\n", indent, ((DataM*)obj)->ptrCount) ;
		printf ("%s    payload  : ", indent) ;
		hexdump (((DataM*)obj)->payload, ((DataM*)obj)->size - offsetof (DataM, payload)) ;
		printf ("\n%s            : ", indent) ;
		chardump (((DataM*)obj)->payload, ((DataM*)obj)->size - offsetof (DataM, payload)) ;
		puts ("") ;
		break ;

	 case 0x51 :
		printf ("%s    id       : 0x%x (SuspIndir)\n", indent, id) ;
		innerDumpObj (level + 1, ((SuspIndir*)obj)->obj) ;
		printf ("%s    arity : %d\n", indent, ((SuspIndir*)obj)->arity) ;
		for (int k = 0 ; k < ((SuspIndir*)obj)->arity ;  k++)
			innerDumpObj (level + 1, ((SuspIndir*)obj)->a [k]) ;
		break ;

	 default :
		printf ("%s    id  : 0x%x : ", indent, id) ;
		hexdump ((unsigned char *) obj, _objSize (obj)) ;
		puts ("") ;
	}
}

void	objDump (Obj* obj)
{
	innerDumpObj (0, obj) ;
}
