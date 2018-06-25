#pragma once

// Interface to the DDC runtime.
//   This is imported by generated modules and defines the types and macros
//   that those modules uses.
#include <stdint.h>
#include <stdlib.h>
#include "runtime/Types.h"


// ----------------------------------------------------------------------------
// Get the format field of an object.
static inline uint8_t
_ddcObjectFormat (Obj* obj)
{       return (uint8_t)(obj ->tagFormat & 0x0f);
}


// ----------------------------------------------------------------------------
static inline uint32_t
_ddcBoxedTag (Obj* obj)
{       return obj ->tagFormat >> 8;
}

Obj*    ddcBoxedAlloc           (uint32_t tag, nat_t arity);
nat_t   ddcBoxedSize            (Obj* obj);
nat_t   ddcBoxedSizeFromArity   (nat_t arity);
tag_t   ddcBoxedTag             (Obj* obj);
nat_t   ddcBoxedFields          (Obj* obj);
Obj*    ddcBoxedGetField        (Obj* obj, nat_t ix);
void    ddcBoxedSetField        (Obj* obj, nat_t ix, Obj* x);


// ----------------------------------------------------------------------------
Obj*     ddcRawAlloc            (uint32_t tag, nat_t bytesPayload);
nat_t    ddcRawSize             (Obj* obj);
uint8_t* ddcRawPayload          (Obj* obj);
nat_t    ddcRawPayloadSize      (Obj* obj);

static inline uint8_t*
_ddcRawPayload(Obj* obj)
{       return ((uint8_t*)obj) + 8;
}


// ----------------------------------------------------------------------------
Obj*     ddcSmallAlloc          (uint32_t info, nat_t wordsPayload);
nat_t    ddcSmallSize           (Obj* obj);
uint8_t* ddcSmallPayload        (Obj* obj);
nat_t    ddcSmallPayloadSize    (Obj* obj);

static inline uint8_t*
_ddcSmallPayloadMain(Obj* obj)
{       return ((uint8_t*)obj) + 8;
}

