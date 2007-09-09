
#ifndef _Trauma_Types
#define _Trauma_Types

#include <sys/types.h>
#include <stdbool.h>

typedef u_int32_t		Tag;
typedef void*			FunPtr;

typedef void*	 		Word;
typedef u_int8_t		Word8;
typedef u_int32_t		Word32;
typedef u_int64_t		Word64;

typedef bool			Bool;

typedef int32_t			Int32;
typedef int64_t			Int64;

typedef unsigned int		UInt;
typedef u_int32_t 		UInt32;
typedef u_int64_t		UInt64;

typedef float			Float32;
typedef double			Float64;

typedef char			Char8;		// An 8 bit byte in UTF-8 encoding.

typedef char*			String;


// -----
// Obj
//	All object in the heap are sub-types of this one.
//
typedef struct {
	Tag	tagFlags;
 } Obj;


// ----- 
typedef struct {
	Tag	tagFlags;
	FunPtr	func;
	UInt	arity;
	UInt	args;
	Obj*	a[];
} Thunk;


// -----
// Data
//	A Data object contains pointers to other object.
//
typedef struct {
	Tag	tagFlags;
	UInt	arity;
	Obj*	a[];
} Data;


// -----
// DataR
//	Contains only raw (non-pointer) data.
//
typedef struct {
	Tag	tagFlags;
	UInt	size;		// size of the whole object, in bytes.
	Word8	payload[];
} DataR;


// -----
// DataRS
//	Contains only raw (non-pointer) data, with the object size
//	encoded as part of the tagFlags field.
//
typedef struct {
	Tag	tagFlags;
	Word8	payload[];
} DataRS;


// -----
// DataM
//	A DataM contains both pointer and non-pointer data.
//	All the pointers in the payload are present before the non-pointer data.
//
typedef struct {
	Tag	tagFlags;
	UInt	size;		// size of the whole object, in bytes.
	UInt	ptrCount;	// number of pointers at the start of the payload
	Word8	payload[];
} DataM;


// -----
typedef struct {
	Tag	tagFlags;
	Obj*	obj;		// When representing a Susp this field points to the thunk to be evaluated.
				// When represending an Indir it points to the new object.
	UInt	arity;
	Obj*	a[];
 } Susp;
 
 
// -----
typedef struct {
	Tag	tagFlags;
	Obj*	obj;
 } Forward;


#endif


