// Primitive functions and operators defined by the runtime system.
// These are all foreign imported into the base library.
#pragma once
#include <stdarg.h>
#include "Runtime.h"

// -- Console -----------------------------------------------------------------
Obj*            ddcPrimStdinGetVector           (nat_t len);
void            ddcPrimStdoutPutString          (string_t* str);
void            ddcPrimStdoutPutTextLit         (string_t* str);
void            ddcPrimStdoutPutVector          (Obj* obj);
void            ddcPrimStdoutFlush              (Obj* obj);
void            ddcPrimFailString               (string_t* str);
void            ddcPrimFailNat                  (nat_t x);

// -- Errno -------------------------------------------------------------------
int             ddcPrimErrnoGet                 ();
Obj*            ddcPrimErrnoShowMessage         (int errno_val);

// -- Exception ---------------------------------------------------------------
Obj*            ddcPrimExceptionTry             (Obj* thunk, Obj* handler);
void            ddcPrimExceptionThrow           (Obj* value);

// -- File --------------------------------------------------------------------
void            ddcPrimFileFail                 (const char* fmt, ...);
Obj*            ddcPrimFileRead                 (string_t* path);
void            ddcPrimFileWrite                (string_t* path, Obj* vec);

// -- Parse -------------------------------------------------------------------
void*           ddcPrimParseAddr                (Obj* pObj);
int             ddcPrimParseInt                 (Obj* pObj);
nat_t           ddcPrimParseNat                 (Obj* pObj);
uint8_t         ddcPrimParseWord8               (Obj* pObj);
uint16_t        ddcPrimParseWord16              (Obj* pObj);
uint32_t        ddcPrimParseWord32              (Obj* pObj);
uint64_t        ddcPrimParseWord64              (Obj* pObj);
float32_t       ddcPrimParseFloat32             (Obj* pObj);
float64_t       ddcPrimParseFloat64             (Obj* pObj);

// -- Show --------------------------------------------------------------------
Obj*            ddcPrimShowAddr                 (void*     val);
Obj*            ddcPrimShowInt                  (int       val);
Obj*            ddcPrimShowNat                  (nat_t     val);
Obj*            ddcPrimShowWord8                (uint8_t   val);
Obj*            ddcPrimShowWord16               (uint16_t  val);
Obj*            ddcPrimShowWord32               (uint32_t  val);
Obj*            ddcPrimShowWord64               (uint64_t  val);
Obj*            ddcPrimShowFloat32              (float32_t val);
Obj*            ddcPrimShowFloat64              (float64_t val);

// -- Text --------------------------------------------------------------------
Obj*            ddcTextLitVPrintf               (const char* fmt, va_list ap);
Obj*            ddcTextLitPrintf                (const char* fmt, ...);

// -- Vector ------------------------------------------------------------------
Obj*            ddcPrimVectorAlloc8             (nat_t len);
uint8_t*        ddcPrimVectorPayload8           (Obj*  vec);
