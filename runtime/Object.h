
#ifndef _DDC_Object
#define _DDC_Object

#include "Types.h"
#include <stdio.h>

static inline UInt _getObjFlags	(Obj* obj) { return obj ->tagFlags & 0x0ff; }
static inline UInt _getObjTag	(Obj* obj) { return obj ->tagFlags >> 8; }
static inline UInt _getObjId	(Obj* obj) { return obj ->tagFlags & 0x0ff; }
static inline UInt _getObjArg	(Obj* obj) { return (obj ->tagFlags & 0x0f0) >> 4; }


// ---- Data flags, present in the lowest 8 bits of the 
//	tagFlags field.
//
//      7  6  5  4  3  2  1  0
//      --- arg --  -- obj ---
//
//      X  X  X  X  X  X  0  0  -- Forward / Broken-Heart
//				--	Objects in the heap are aligned on 4-byte boundaries, so
//				--	The lowest two bits of pointers will always be 00.
//
//	X  X  X  X  a  X  X  X  -- Anchor flag. 
//				--	Whether the GC is permitted to move this object.

//	0  0  0  1  a  0  0  1	-- Thunk
//	0  0  1  0  a  0  0  1  -- Data
//      0  0  1  1  a  0  0  1  -- DataR
//	0  1  0  0  a  0  0  1  -- DataM
//      0  1  0  1  a  0  0  1  -- Susp
//	0  1  1  1  a  0  0  1	-- Reserve for bitmapped.
//				--	Include ptr to layout bitmap in object.
//
//      -- size --  a  0  1  1  -- DataRS	
//				--	size contains the number of 4 byte words in the payload.
//
//


// Object type
enum _ObjType
{
	_ObjTypeUnknown,
	_ObjTypeForward,
	_ObjTypeThunk,
	_ObjTypeData,
	_ObjTypeDataR,
	_ObjTypeDataM,
	_ObjTypeSusp,
	_ObjTypeMapped,
	_ObjTypeDataRS
};



// Object mode
enum _ObjMode
{
	_ObjModeForward		= 0x00,
	_ObjModeFixed		= 0x01,
	_ObjModeDataRS		= 0x03
};

#define _MaskObjMode		0x03


// Fixed types
enum _ObjFixed
{
	_ObjFixedThunk		= 0x11,
	_ObjFixedData		= 0x21,
	_ObjFixedDataR		= 0x31,
	_ObjFixedDataM		= 0x41,
	_ObjFixedSusp		= 0x51,
	_ObjFixedMapped		= 0x71
};

#define _MaskObjFixed		0xf7


// Flags
enum _ObjFlag
{
	_ObjFlagAnchored	= 0x08
};

#define _MaskObjAnchored	(~_objFlagAnchored)


enum _ObjType	
	_objType 	(Obj* obj);

enum _ObjType	
	_objTypeFixed 	(Obj* obj);

UInt	_objSize 	(Obj* obj);

bool	_objIsAnchored	(Obj* obj);


#endif
