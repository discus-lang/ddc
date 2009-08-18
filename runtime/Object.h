
// Defines the layout of objects in the heap.

#ifndef _DDC_Object
#define _DDC_Object

#include "Types.h"
#include <stdio.h>


// Object Layout ----------------------------------------------------------------------------------
// 
//	An object in the heap consists of a 32bit header word followed
//	the payload. 
//
//	The header's upper 24 bits contain the tag value of the particular
//	data constructor that created the object, and the lower 8 bits
//	describe the structure of the object to the garbage collector.
//
//	Example object:
//	--------------------------------
//	|         TAG         | FORMAT |  Header Word
//	--------------------------------
//	|           PAYLOAD0           |
//	--------------------------------
//	|           PAYLOAD1           |
//	--------------------------------
//	|            ...               |
//


// Special Tags -----------------------------------------------------------------------------------
//	Some values of the TAG field in the header are special.

#define _null		0

// The first tag value that can be used in a regular data type.
#define _tagBase	0

// Objects that are thunks, suspensions or indirections always have 
// 	the following values in their tag fields. These tags are
//	reserved and cannot be used for other objects.
// 
#define _tagThunk	0x0ffffff
#define _tagSusp	0x0fffffe
#define _tagIndir	0x0fffffd

// Define the tags of the False and True data constructors. 
//	The RTS sometimes needs to test for these directly.
#define _tagFalse       (_tagBase + 0)
#define _tagTrue        (_tagBase + 1)


// Format field -----------------------------------------------------------------------------------
//
//	The format field describes the format/layout of the heap object.
//
//  bit 7  6  5  4  3  2  1  0
//      -- arg ---  -- obj ---
//      X  X  X  X  X  X  0  0  -- Forward / Broken-Heart
//	X  X  X  X  a  X  X  X  -- Anchor flag
//	0  0  0  1  a  0  0  1	-- Thunk
//	0  0  1  0  a  0  0  1  -- Data
//      0  0  1  1  a  0  0  1  -- DataR
//	0  1  0  0  a  0  0  1  -- DataM
//      0  1  0  1  a  0  0  1  -- Susp
//      -- size --  a  0  1  1  -- DataRS	
//	
// 	* GC Forwarding / Broken-Heart pointers.
//	  During garbage collection, after the GC copies an object to the "to-space"
//        its header in the "from-space" is overwritten with a pointer to where
//        the "to-space" version of the object is.
//
//	  We can identify these pointers because their lowest 2 bits are always 00.
//        This is because objects in the heap are always 4-byte aligned.
//
//        For all other values of the format field, we ensure the lowest two bits
//	  are not 00.
//
//	* Anchor flag
//	  If bit 3 in the format field is set then the GC is not permitted to move
//	  the object. This is useful when the object has been allocated by malloc
//	  and exists outside the DDC runtime's garbage collected heap.
//
//	* Data{_,R,M,RS}
//	  The objects represent constructed data, and their format is described below.
//	  For the RS (raw-small) object we use some of the bits in the header word
//	  to encode length of the payload, in 4-byte words.
//
//	The -obj- (object mode) portion of the format field can be used to determine
//	if the object is a forwarding pointer, has a fixed value for its format field,
//	or is a DataRS object.


// All the possible object types / formats.
enum _ObjType
{
	_ObjTypeUnknown,
	_ObjTypeForward,
	_ObjTypeThunk,
	_ObjTypeData,
	_ObjTypeDataR,
	_ObjTypeDataM,
	_ObjTypeSusp,
	_ObjTypeDataRS
};


// Whether the object is:
//	a forwarding pointer, 
//	has a fixed format, 
//	or is a DataRS object that has its payload size encoded in format field as well.
//
enum _ObjMode
{
	_ObjModeForward		= 0x00,
	_ObjModeFixed		= 0x01,
	_ObjModeDataRS		= 0x03
};

// Use this mask to select the object mode portion of the format field.
#define _MaskObjMode		0x03


// If the object has a fixed format field (ie, has _ObjModeFixed)
//	then we can determine the format of the rest of the object
//	by masking the format field with the following mask and testing
//	against this enum.
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


// Optional flags in the format field.
enum _ObjFlag
{
	_ObjFlagAnchored	= 0x08
};

#define _MaskObjAnchored	(~_objFlagAnchored)


// Objects ----------------------------------------------------------------------------------------
// 	All the objects in the heap are subtypes of this one.
//	that is, they all contain the header word (tag + flags)
//
//	All objects are an integral number of machine words long, and aligned
//	on machine word boundaries.
//
typedef struct {
	Tag	tagFlags;
 } Obj;


// Partial Applications ---------------------------------------------------------------------------
// A Thunk that represents a partial application.
typedef struct {
	Tag	tagFlags;
	FunPtr	func;		// Pointer to the supercombinator of the function.
	UInt	arity;		// The arity of the supercombinator.
	UInt	args;		// Number of arg pointers stored in this thunk.
				//	(ie, the length of the "a" array)
	Obj*	a[];		// Pointers to the arguments.
} Thunk;


// Suspensions and Forwards -----------------------------------------------------------------------
// A suspension of a fully applied function application.
//	This should probably be merged in to Thunk above.
//
typedef struct {
	Tag	tagFlags;
	Obj*	obj;		// When representing a Susp this field points to the
				//	thunk to be evaluated.
				// When represending an Indir it points to the new object.
	UInt	arity;		// Arity of the supercombinator
	Obj*	a[];		// Pointers to the arguments.
 } Susp;
 
 
// A forwarding pointer. (also called an indirection)
//	When a Susp is evaluated, it is overwritten by a Forward which points
//	to the result object.
//
typedef struct {
	Tag	tagFlags;
	Obj*	obj;		// Pointer to the result object.
 } Forward;


// Data Objects -----------------------------------------------------------------------------------

// A general Data object.
//	It contains a tag (in the header word), and pointers to other objects.
typedef struct {
	Tag	tagFlags;
	UInt	arity;		// Arity of the data constructor.
				//	(ie, the length of the "a" array)
	Obj*	a[];
} Data;


// A raw data object that only contains non-pointer data.
typedef struct {
	Tag	tagFlags;
	UInt	size;		// Size of the whole object, in bytes.
	Word8	payload[];	// Some unboxed binary data, which isn't interpreted
				//	by the GC.
} DataR;


// A small raw data object contains only non-pointer data.
//	The object size is encoded as part of tagFlags field / header word.
//	This saves us from having to include a separate arity field.
typedef struct {
	Tag	tagFlags;
	Word8	payload[];
} DataRS;


// A mixed data object contains both pointer and non-pointer data.
//	All the pointers in the payload are present before the non-pointer data.
//
typedef struct {
	Tag	tagFlags;
	UInt	size;		// Size of the whole object, in bytes.
	UInt	ptrCount;	// The number of pointers at the start of the payload
	Word8	payload[];	// Contains ptrCount pointers, then some uninterpreted data.
} DataM;


// Header Utils -----------------------------------------------------------------------------------

// Get the tag portion of this object's header word.
static inline UInt _getObjTag	(Obj* obj) 
{ 	
	return obj ->tagFlags >> 8; 
}

// Get the arg portion of this object's format field.
static inline UInt _getObjArg	(Obj* obj) 
{
	return (obj ->tagFlags & 0x0f0) >> 4; 
}

// Get this objects header word.
static inline UInt _getObjFlags	(Obj* obj) 
{ 	
	return obj ->tagFlags & 0x0ff; 
}

// Get this object's header word.
static inline UInt _getObjId	(Obj* obj) 
{
	return obj ->tagFlags & 0x0ff; 
}

// Determine the type of this object.
enum _ObjType	_objType 	(Obj* obj);

// Determine the type of an object, knowing that it has a fixed format field layout.
//	(ie, it's not a forwarding pointer or a DataRS)
enum _ObjType	_objTypeFixed 	(Obj* obj);

// Determine the total size of this object.
UInt		_objSize 	(Obj* obj);

// Determine whether the object is anchored and cannot
//	be moved by the garbage collector.
bool		_objIsAnchored	(Obj* obj);


#endif
