
// Utils for determining the layout of objects in the heap.
#include "Runtime.h"
#include <stddef.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

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
size_t	_objSize (Obj* obj)
{	size_t s = 0;

	switch (_objType(obj)) {
	 case _ObjTypeThunk:
		s = sizeof (Thunk) + sizeof(Obj*) * ((Thunk*)obj) ->arity;
		break ;

	 case _ObjTypeData:
		s = sizeof (Data)  + sizeof(Obj*) * ((Data*) obj) ->arity;
		break ;

	 case _ObjTypeDataR:
		s = ((DataR*)obj) ->size;
		break ;

	 case _ObjTypeDataRS:
		s = sizeof (DataRS) + _getObjArg(obj) * 4;
		break ;

	 case _ObjTypeDataM:
		s = ((DataM*)obj) ->size;
		break ;

	 case _ObjTypeSuspIndir:
		s = sizeof (SuspIndir) + sizeof(Obj*) * ((SuspIndir*)obj) ->arity;
		break ;

	 default:
		_ERROR ("Unknown object type.\n");
		_ERROR ("  obj      = %p\n",	obj);
		_ERROR ("  tagFlags = 0x%08x\n", 	obj ->tagFlags);
		_PANIC ("Can't take the size of an unknown object.\n");
		break;
	}

	if (s == 0)
		_PANIC ("Object %p should have size > 0.\n", obj) ;

	return s ;
}

// Determine whether the object is anchored and cannot
//	be moved by the garbage collector.
bool	_objIsAnchored (Obj* obj)
{
	return	(_getObjFlags (obj) & _ObjFlagAnchored) ? true : false;
}

//------------------------------------------------------------------------------
// Functions for debugging the runtime.


// Dump data to this file in hex format.
static void
hexdump (FILE *f, Word8 *ptr, int len)
{
	fprintf (f, "[ ");

	for (int k = 0; k < len;  k++)
		fprintf (f, "%02x ", ptr [k]);

	fprintf (f, "]\n");
}


// Dump printable characters from buffer into file.
static void
strdump (FILE *f, Word8 *ptr, int len)
{
	putchar ('"');
	for (int k = 0; k < len;  k++)
		putchar (isprint (ptr [k]) ? ptr [k] : '.');

	fprintf (f, "\"\n");
}


// Dump out a single object / or all objects reachable from this one.
static void
innerDumpObj
	( FILE *f		// file to dump o
	, int level		// indent level
	, Obj* obj		// root object
	, bool recurse)		// whether to recurse into other objects.
{
	int indent = 2 * level + (level == 0 ? 1 : 3);
	int tag, id, size;

	size	= _objSize   (obj);
	tag	= _getObjTag (obj);
	id	= _getObjId  (obj);

	switch (id & _MaskObjAnchored) {
	 case _ObjFixedThunk :
		fprintf (f, "%*cThunk @ %p (%d bytes)\n", indent, ' ', obj, size);
		fprintf (f, "%*c  , tag   = 0x%06x\n",	  indent, ' ', tag);
		fprintf (f, "%*c  , func  = %p\n",	  indent, ' ', ((Thunk*)obj)->func);
		fprintf (f, "%*c  , arity = %d\n",	  indent, ' ', ((Thunk*)obj)->arity);
		fprintf (f, "%*c  , args  = %d\n",	  indent, ' ', ((Thunk*)obj)->args);

		for (uint32_t k = 0 ; k < ((Thunk*)obj)->args ;  k++) {
			if (recurse)
				innerDumpObj (f, level + 1, ((Thunk*)obj)->a [k], recurse);
			else
				fprintf (f, "%*c  , a [%3d] = %p\n", indent, ' ', k, ((Thunk*)obj)->a [k]);
		}
		fprintf (f, "%*c  }\n", indent, ' ') ;

		break;

	 case _ObjFixedData :
		fprintf (f, "%*cData @ %p (%d bytes) {\n", indent, ' ', obj, size);
		fprintf (f, "%*c  , tag     = 0x%06x\n",   indent, ' ', tag);
		fprintf (f, "%*c  , arity   = %d\n",       indent, ' ', ((Data*)obj)->arity);

		for (uint32_t k = 0 ; k < ((Data*)obj)->arity ;  k++) {
			if (recurse)
				innerDumpObj (f, level + 1, ((Data*)obj)->a [k], recurse) ;
			else
				fprintf (f, "%*c  , a [%3d] = %p\n", indent, ' ', k, ((Data*)obj)->a [k]) ;
		}

		fprintf (f, "%*c  }\n", indent, ' ') ;
		break ;

	 case _ObjFixedDataR :
		fprintf (f, "%*cDataR @ %p (%d bytes) {\n", indent, ' ', obj, size);
		fprintf (f, "%*c  , tag     = 0x%06x\n",    indent, ' ', tag);
		fprintf (f, "%*c  , size    = %d\n",        indent, ' ', ((DataR*)obj)->size);
		fprintf (f, "%*c  , payload = ",            indent, ' ');

		hexdump (f, 	((DataR*)obj)->payload,
				((DataR*)obj)->size - offsetof (DataR, payload));

		fprintf (f, "%*c                  -> ", indent, ' ') ;

		strdump (f, 	((DataR*)obj)->payload,
				((DataR*)obj)->size - offsetof (DataR, payload));

		fprintf (f, "%*c  }\n", indent, ' ') ;
		break ;

	 case _ObjFixedDataM :
		fprintf (f, "%*cDataM @ %p (%d bytes) {\n", indent, ' ', obj, size);
		fprintf (f, "%*c  , tag      = 0x%06x\n",   indent, ' ', tag);
		fprintf (f, "%*c  , size     = %d\n",       indent, ' ', ((DataM*)obj)->size);
		fprintf (f, "%*c  , ptrCount = %d\n",       indent, ' ', ((DataM*)obj)->ptrCount);
		fprintf (f, "%*c  , payload  = ",           indent, ' ');

		hexdump (f,	((DataM*)obj)->payload,
			    	((DataM*)obj)->size - offsetof (DataM, payload));

		fprintf (f, "%*c                  -> ",     indent, ' ');

		strdump (f, 	((DataM*)obj)->payload,
				((DataM*)obj)->size - offsetof (DataM, payload));

		fprintf (f, "%*c  }\n", indent, ' ');
		break;

	 case _ObjFixedSuspIndir :
		fprintf (f, "%*cSuspIndir @ %p (%d bytes) {\n", indent, ' ', obj, size);
		fprintf (f, "%*c  , tag   = 0x%06x\n",          indent, ' ', tag);

		innerDumpObj (f, level + 1, ((SuspIndir*)obj)->obj, recurse);

		fprintf (f, "%*c    arity = %d\n", indent, ' ', ((SuspIndir*)obj)->arity);

		for (uint32_t k = 0; k < ((SuspIndir*)obj)->arity;  k++)
			innerDumpObj (f, level + 1, ((SuspIndir*)obj)->a [k], recurse);

		fprintf (f, "%*c  }\n", indent, ' ') ;

		break ;

	 default :
		fprintf (f, "%*cUnknown @ %p (%d bytes) {\n",   indent, ' ', obj, size);
		fprintf (f, "%*c  , tag = 0x%06x\n",            indent, ' ', tag);
		fprintf (f, "%*c  , id  : 0x%x : ",             indent, ' ', id);

		hexdump (f, (unsigned char *) obj, size);

		fprintf (f, "%*c  }\n", indent, ' ');
	}
}


// Dump a single object to a file.
void	objDump (FILE *f, Obj* obj)
{
	innerDumpObj (f, 0, obj, false);
}


// Dump all objects reachable from this one to a file.
void	objDumpGraph (FILE *f, Obj* obj)
{
	innerDumpObj (f, 0, obj, true);
}

