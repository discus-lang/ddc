#pragma once

// Interface to the DDC runtime.
//   This is imported by generated modules and defines the types and macros
//   that those modules uses.
#include <stdint.h>
#include <stdlib.h>
#include "runtime/Types.h"


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

