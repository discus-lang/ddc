
#include "../Runtime.h"
#include <stdlib.h>
#include <stdarg.h>


static void _rdumpObj	(FILE* file, Obj* obj, int indent) ;


static int
indent_fprintf (int indent, FILE *file, const char *fmt, ...)
{
	va_list ap;
	int count;

	va_start (ap, fmt);
	count = fprintf (file, "%*c", indent, ' ') ;
	count += vfprintf (file, fmt, ap) ;
	va_end (ap);

	return count;
}


// Dump a thunk
static void
_rdumpThunk 	(FILE* file, Obj* obj, int indent)
{
	Thunk* thunk	= (Thunk*)obj;

	fprintf (file, "Thunk\n");
	indent_fprintf (indent, file, " { tag     = 0x%06x\n", _getObjTag (obj));
	indent_fprintf (indent, file, " , flags   = 0x%02x\n", _getObjFlags (obj));
	indent_fprintf (indent, file, " , func    = %p\n", thunk ->func);
	indent_fprintf (indent, file, " , arity   = %d\n", thunk ->arity);
	indent_fprintf (indent, file, " , args    = %d\n", thunk ->args);

	for (uint32_t i = 0; i < thunk ->args; i++)
		indent_fprintf (indent, file, "%*c , a[%2d]   = %p\n", i, thunk ->a[i]);

	indent_fprintf (indent, file, " }\n");
}


// Dupm a suspension
static void
_rdumpSusp	(FILE* file, Obj* obj, int indent)
{
	SuspIndir* susp	= (SuspIndir*)obj;

	fprintf (file, "Susp\n");
	indent_fprintf (indent, file, " { tag     = 0x%06x (%s)\n"
		, _getObjTag (obj)
		, (_getObjTag (obj) == _tagSusp) ? "susp" : "indir");

	indent_fprintf (indent, file, " , obj     = ");
	_rdumpObj (file, susp->obj, indent + 14);

	indent_fprintf (indent, file, " , airity  = %d\n", susp ->arity);

	for (uint32_t i = 0; i < susp->arity; i++) {
		indent_fprintf (indent, file, " , a[%2d]   = ", i);
		_rdumpObj (file, susp->a[i], indent + 14);
	}

	indent_fprintf (indent, file, " }\n");
}


// Dump data with pointers
static void
_rdumpData	(FILE* file, Obj* obj, int indent)
{
	Data* data	= (Data*)obj;

	fprintf (file, "Data\n");
	indent_fprintf (indent, file, "{ tag     = 0x%06x\n", _getObjTag (obj));
	indent_fprintf (indent, file, ", arity   = %d\n", data ->arity);


	for (uint32_t i = 0; i < data->arity; i++) {
		indent_fprintf (indent, file, ", a[%2d]   = ", i);
		_rdumpObj (file, data->a[i], indent + 14);
	}

	indent_fprintf (indent, file, "}\n");
}


// Dump raw data
static void
_rdumpDataR	(FILE* file, Obj* obj, int indent)
{
	DataR*	data	= (DataR*)obj;
	indent = 0 ;

	fprintf (file, "DataR\n");
	fprintf (file, "  { tag     = 0x%06x\n", _getObjTag (obj));
	fprintf (file, "  , size    = %d\n",     data ->size);

	fprintf (file, "  }\n");
}


// Dump small raw data
static void
_rdumpDataRS 	(FILE* file, Obj* obj, int indent)
{
	DataRS* data	= (DataRS*)obj;

	fprintf (file, "DataRS\n");
	indent_fprintf (indent, file, " { tag     = 0x%06x\n", _getObjTag (obj));
	indent_fprintf (indent, file, " , payload = [");

	size_t	payloadSize
			= _getObjArg (obj) * 4;

	for (uint32_t i = 0; i < payloadSize; i++)
		fprintf (file, " %02x", data ->payload[i]);

	fprintf (file, " ]\n");

	indent_fprintf (indent, file, " }\n");
}


// Dump mixed pointer / non-pointer data
static void
_rdumpDataM 	(FILE* file, Obj* obj, int indent)
{
	DataM*	data	= (DataM*)obj;

	fprintf (file, "DataM\n");
	indent_fprintf (indent, file, " { tag      = 0x%06x\n", _getObjTag (obj));
	indent_fprintf (indent, file, " , size     = %d\n", data ->size);
	indent_fprintf (indent, file, " , ptrCount = %d\n", data ->ptrCount);

	// print out obj pointers.
	Obj** payloadObj	= (Obj**) &(data ->payload);

	for (uint32_t i = 0; i < data ->ptrCount; i++)
		fprintf (file, "  , a[%2d]   = %p\n", i, payloadObj[i]);

	fprintf (file, "  }\n");
}

// Dump an object to a file
static void
_rdumpObj	(FILE* file, Obj* obj, int indent)
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

	 case _ObjTypeThunk:	_rdumpThunk (file, obj, indent);	return;
	 case _ObjTypeData:	_rdumpData  (file, obj, indent);	return;
	 case _ObjTypeDataR:	_rdumpDataR (file, obj, indent);	return;
	 case _ObjTypeDataM:	_rdumpDataM (file, obj, indent);	return;
	 case _ObjTypeSuspIndir: _rdumpSusp  (file, obj, indent);	return;

	 case _ObjTypeDataRS:	_rdumpDataRS (file, obj, indent);	return;
	}

	_PANIC ("_rdumpObj: bad object type.\n");
}

void
_recDumpObj	(FILE* file, Obj* obj)
{
	_rdumpObj	(file, obj, 1) ;
}
