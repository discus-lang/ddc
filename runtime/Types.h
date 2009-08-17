
#ifndef _DDC_Types
#define _DDC_Types

#include <stdint.h>
#include <stdbool.h>

// The types that the generated C code uses.
//	These names are also used as the names of unboxed types
//	In the Disciple source code.

// Object tags
//	Records the index of the constructor in a data type.
typedef uint32_t		Tag;

// Function pointers
typedef void*			FunPtr;

// Booleans
typedef bool			Bool;

// Unsigned words
//	We use these instead of UInt when the value shouldn't nessesaraly
//	interpreted as a number. ie, bulk data that C programmers would 
//	normaly use void* for.
//
typedef void*	 		Word;
typedef uint8_t			Word8;
typedef uint32_t		Word32;
typedef uint64_t		Word64;

// Signed integers
typedef int32_t			Int32;
typedef int64_t			Int64;

// Unsigned integers
typedef unsigned int		UInt;
typedef uint32_t 		UInt32;
typedef uint64_t		UInt64;

// Flloaring point numbers
typedef float			Float32;
typedef double			Float64;

// Characters
//	We intend to use Char32 for unicode characters,
//	but they're not implemented yet.
typedef char			Char8;
typedef uint32_t		Char32;		

// Strings
//	Null terminated C-strings.
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
#error "BITS must be defined and must be either 32 or 64. \
	Set this value in make/config.mk and src/Config/Config.hs.platform for your platform."

#endif


// Objects ----------------------------------------------------------------------------------------
// All the objects in the heap are subtypes of this one.
//	that, they all start with the tag/flags field.
//
//	The tagFlags field contains both the constructor tag, and header
//	bits the GC uses to identify the object.
//
//	See Object.h for a description of the header bits.
//
//	All objects are an integral number of machine words, and aligned
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


#endif


