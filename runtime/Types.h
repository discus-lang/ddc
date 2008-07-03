
#ifndef _DDC_Types
#define _DDC_Types

#include <sys/types.h>
#include <stdbool.h>


// Define the types that the generated C code uses.

// Object tags
typedef u_int32_t		Tag;

typedef void*			FunPtr;

typedef bool			Bool;

typedef void*	 		Word;
typedef u_int8_t		Word8;
typedef u_int32_t		Word32;
typedef u_int64_t		Word64;

typedef int32_t			Int32;
typedef int64_t			Int64;

typedef unsigned int		UInt;
typedef u_int32_t 		UInt32;
typedef u_int64_t		UInt64;

typedef float			Float32;
typedef double			Float64;

typedef char			Char8;
typedef u_int32_t		Char32;		

typedef char*			String;

// For machines with 32 bit pointers
#if BITS == 32
typedef int32_t			MachineInt;
typedef u_int32_t		MachineWord;
typedef u_int32_t		SizePtr;
typedef u_int16_t		HalfPtr;

// For machines with 64 bit pointers
#elif BITS == 64
typedef int64_t			MachineInt;
typedef u_int64_t		MachineWord;
typedef u_int64_t		SizePtr;
typedef u_int32_t		HalfPtr;

#endif


// All the objects in the heap are subtypes of this one.
//	ie, they all start with the tag/flags field.
typedef struct {
	Tag	tagFlags;
 } Obj;


// A Thunk represents a partial application.
typedef struct {
	Tag	tagFlags;
	FunPtr	func;		// Pointer to the supercombinator.
	UInt	arity;		// The airity of the super.
	UInt	args;		// Number of args actually in this thunk.
	Obj*	a[];
} Thunk;


// A susp is suspension of a fully applied function application.
//	This should probably be merged in to Thunk above.
typedef struct {
	Tag	tagFlags;
	Obj*	obj;		// When representing a Susp this field points to the thunk to be evaluated.
				// When represending an Indir it points to the new object.
	UInt	arity;
	Obj*	a[];
 } Susp;
 
 
// A forwarding pointer, created by overwriting a susp after the function has evaluated.
typedef struct {
	Tag	tagFlags;
	Obj*	obj;
 } Forward;


// A Data object contains pointers to other objects.
typedef struct {
	Tag	tagFlags;
	UInt	arity;
	Obj*	a[];
} Data;


// A raw data object contains only non-pointer data.
typedef struct {
	Tag	tagFlags;
	UInt	size;		// size of the whole object, in bytes.
	Word8	payload[];
} DataR;


// A small raw data object contains only non-pointer data,
//	and the object size is encoded as part of tagFlags field.
typedef struct {
	Tag	tagFlags;
	Word8	payload[];
} DataRS;


// A mixed data object contains both pointer and non-pointer data.
//	All the pointers in the payload are present before the non-pointer data.
//
typedef struct {
	Tag	tagFlags;
	UInt	size;		// size of the whole object, in bytes.
	UInt	ptrCount;	// the number of pointers at the start of the payload
	Word8	payload[];
} DataM;


#endif


