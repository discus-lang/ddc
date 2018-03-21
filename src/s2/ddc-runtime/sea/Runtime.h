#pragma once

// Interface to the DDC runtime.
//   This is imported by generated modules and defines the types and macros
//   that those modules uses.
#include <stdint.h>
#include <stdlib.h>


// -- Types -------------------------------------------------------------------
// Boolean type.
typedef int     bool_t;

// An unsigned natural number.
//   Used for object sizes and field counts.
//   Big enough to represent the number of allocatable bytes.
typedef size_t  nat_t;

// Define int_t to make things look consistent.
typedef int     int_t;

// Generic address type.
//   #ifdef because Cygwin already defines it.
#ifndef __addr_t_defined
typedef uint8_t* addr_t;
#endif

// A constructor tag.
typedef uint32_t tag_t;

// A UTF8 string.
typedef char    string_t;

// Floating point types.
typedef float   float32_t;
typedef double  float64_t;


// -- Object Structures -------------------------------------------------------
// Object
// A General Object.
//   All objects contain the tag and format field as the first 32-bit word.
//   The following is a supertype of the others.
typedef struct
{        uint32_t  tagFormat;
} Obj;


// Get the format field of an object.
static inline uint8_t
_ddcObjectFormat (Obj* obj)
{       return (uint8_t)(obj ->tagFormat & 0x0f);
}


// ----------------------------------------------------------------------------
// A Boxed Data Object.
//   The payload contains pointers to other heap objects.
typedef struct
{       uint32_t  tagFormat;    // Constructor tag and format field.
        uint32_t  arity;        // Number of fields in object.
        Obj*      payload[];
} ObjBoxed;

static inline uint32_t
_ddcBoxedTag (Obj* obj)
{       return obj ->tagFormat >> 8;
}

Obj*    ddcBoxedAlloc    (uint32_t tag, nat_t arity);
nat_t   ddcBoxedFields   (Obj* obj);
Obj*    ddcBoxedGetField (Obj* obj, nat_t ix);
void    ddcBoxedSetField (Obj* obj, nat_t ix, Obj* x);


// ----------------------------------------------------------------------------
// A Raw Data Object.
//   A raw data object does not contain heap pointers that need to be traced
//   by the garbage collector.
typedef struct
{       uint32_t  tagFormat;    // Constructor tag and format field.
        uint32_t  size;         // Size of the whole object, in bytes.
        uint8_t   payload[];    // Raw data that does not contain heap pointers.
} ObjRaw;

Obj*     ddcRawAlloc        (uint32_t tag, nat_t payloadLength);
uint8_t* ddcRawPayload      (Obj* obj);
nat_t    ddcRawPayloadSize  (Obj* obj);

static inline uint8_t*
_ddcRawPayload(Obj* obj)
{       return ((uint8_t*)obj) + 8;
}


// ----------------------------------------------------------------------------
// A Small Raw object.
//   The object size is encoded as part of format field.
//    This saves us from needing to include a separate arity field.
typedef struct
{       uint32_t  tagFormat;    // Constructor tag and format field.
        uint8_t   payload[];    // Raw data that does not contain heap pointers.
} ObjSmall;

Obj*     ddcSmallAlloc      (uint32_t tag, nat_t payloadLength);
uint8_t* ddcSmallPayload    (Obj* obj);
nat_t    ddcSmallPayloadSize(Obj* obj);

