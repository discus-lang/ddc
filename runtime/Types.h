
#ifndef _DDC_Types
#define _DDC_Types

#include <stdint.h>
#include <stdbool.h>

// The types that the generated C code uses.
//	These names are also used as the names of unboxed types
//	In the Disciple source code.

// Function pointers
typedef void			(*FunPtr) (void);

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
//	Null terminated C-strings, treated as an abstract type.
//	(Ptr String) in Disciple == (char*) in C.
typedef char			String;


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

#endif


