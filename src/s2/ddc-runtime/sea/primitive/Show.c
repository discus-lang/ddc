#pragma once
#include <string.h>
#include <inttypes.h>
#include <alloca.h>
#include "Runtime.h"

// Show functions.
//   We provide a binding to the stdlib versions of these instead of defining
//   our own in the base library. DDC isn't yet good enough to eliminate the
//   intermediate boxings/unboxings, and we don't want to pay the performance
//   penalty for demos that write a lot of numeric output.
//
//   The task of pretty printing floating point numbers well also isn't
//   straightforward, so if we need to rely on stdlib for floats we might
//   as well do it for all numeric types.

#define _DDC_MAKE_PRIM_SHOW_TYPE(typeName,typeSpec,format,nBuf) \
 Obj* ddcPrimShow##typeName (typeSpec x) \
 { \
        string_t* pBuf  = alloca(nBuf); \
        snprintf(pBuf, nBuf - 1, format, x); \
        nat_t n         = strlen(pBuf); \
   \
        Obj* pObj       = ddcAllocRaw (0, 4 + n + 1); \
        uint8_t*  p8    = _ddcPayloadRaw(pObj); \
        uint32_t* pLen  = (uint32_t*)p8; \
        string_t* pStr  = (string_t*)(p8 + 4); \
   \
        memcpy(pStr, pBuf, n + 1); \
        *pLen           = n; \
        return pObj; \
 }

_DDC_MAKE_PRIM_SHOW_TYPE(Addr,   void*,     "%p",          24);
_DDC_MAKE_PRIM_SHOW_TYPE(Int,    int,       "%d",          24);
_DDC_MAKE_PRIM_SHOW_TYPE(Nat,    nat_t,     "%zu",         24);
_DDC_MAKE_PRIM_SHOW_TYPE(Word8,  uint8_t,   "%#01" PRIx8,  24);
_DDC_MAKE_PRIM_SHOW_TYPE(Word16, uint16_t,  "%#02" PRIx16, 24);
_DDC_MAKE_PRIM_SHOW_TYPE(Word32, uint32_t,  "%#04" PRIx32, 24);
_DDC_MAKE_PRIM_SHOW_TYPE(Word64, uint64_t,  "%#08" PRIx64, 24);
_DDC_MAKE_PRIM_SHOW_TYPE(Float32,float32_t, "%f",          24);
_DDC_MAKE_PRIM_SHOW_TYPE(Float64,float64_t, "%.15lg",       24);


