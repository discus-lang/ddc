#pragma once
#include <string.h>
#include <inttypes.h>
#include "Runtime.h"

// Parse functions.
#define _DDC_MAKE_PRIM_PARSE_TYPE(typeName,typeSpec,format) \
 typeSpec ddcPrimParse##typeName (Obj* pObj) \
 { \
        uint8_t*  p8    = (uint8_t*)_ddcPayloadRaw(pObj); \
        uint32_t* pLen  = (uint32_t*)p8; \
        string_t* pStr  = (string_t*)(p8 + 4); \
        typeSpec  x; \
   \
        sscanf(pStr, format, &x); \
        return x; \
 }

_DDC_MAKE_PRIM_PARSE_TYPE(Addr,   void*,     "%p");
_DDC_MAKE_PRIM_PARSE_TYPE(Int,    int,       "%d");
_DDC_MAKE_PRIM_PARSE_TYPE(Nat,    nat_t,     "%zu");
_DDC_MAKE_PRIM_PARSE_TYPE(Word8,  uint8_t,   "%" SCNx8);
_DDC_MAKE_PRIM_PARSE_TYPE(Word16, uint16_t,  "%" SCNx16);
_DDC_MAKE_PRIM_PARSE_TYPE(Word32, uint32_t,  "%" SCNx32);
_DDC_MAKE_PRIM_PARSE_TYPE(Word64, uint64_t,  "%" SCNx64);
_DDC_MAKE_PRIM_PARSE_TYPE(Float32,float32_t, "%f");
_DDC_MAKE_PRIM_PARSE_TYPE(Float64,float64_t, "%lg");

