
#ifndef _DDC_Types
#define _DDC_Types

#include <stdint.h>
#include <stdbool.h>


// Define the types that the generated C code uses.

// Object tags
typedef uint32_t		Tag;

typedef void*			FunPtr;

typedef bool			Bool;

typedef void*	 		Word;
typedef uint8_t		Word8;
typedef uint32_t		Word32;
typedef uint64_t		Word64;

typedef int32_t			Int32;
typedef int64_t			Int64;

typedef unsigned int		UInt;
typedef uint32_t 		UInt32;
typedef uint64_t		UInt64;

typedef float			Float32;
typedef double			Float64;

typedef char			Char8;
typedef uint32_t		Char32;		

typedef char*			String;

// For machines with 32 bit pointers
#if BITS == 32
typedef int32_t			MachineInt;
typedef uint32_t		MachineWord;
typedef uint32_t		SizePtr;
typedef uint16_t		HalfPtr;

// For machines with 64 bit pointers
#elif BITS == 64
typedef int64_t			MachineInt;
typedef uint64_t		MachineWord;
typedef uint64_t		SizePtr;
typedef uint32_t		HalfPtr;

#else
#error "BITS must be defined and must be either 32 or 64.  Set this value in make/config.mk and src/Config/Config.hs.platform for your platform."

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


