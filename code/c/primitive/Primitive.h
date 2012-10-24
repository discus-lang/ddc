// Primitive operations that generated C code uses.
//   These should all be static inlined so we can compile programs without
//   linking against external code. Operations that need manifest object
//   code should be defined somewhere else, and preferably imported
//   by the foreign-function interface.
#pragma once
#include <stdio.h>
#include "Runtime.h"


// Store Primops --------------------------------------------------------------
extern addr_t _DDC_Runtime_heapTop;
extern addr_t _DDC_Runtime_heapMax;


// Create the initial store.
static inline
void    _CREATE (nat_t bytes)
{
        _DDC_Runtime_heapTop    = malloc (bytes);
        _DDC_Runtime_heapMax    = _DDC_Runtime_heapTop + bytes;        
}

// Allocate some space in the store
static inline 
addr_t _ALLOC (nat_t bytes) 
{       
        addr_t obj              = _DDC_Runtime_heapTop;
        _DDC_Runtime_heapTop    = _DDC_Runtime_heapTop + bytes;
        return obj;
}       

// Get the size of a type.
#define _SIZE(type)                     sizeof(type)

// Read from a field of an Object.
//   We use an explicit macro to make it easier to see what is happening in
//   the generated code.
#define _READ(type,addr,offset)         (*((type *)(addr + offset)))

// Write to a field of an Object.
//   We use an explicit macro to make it easier to see what is happening in
//   the generated code.
#define _WRITE(type,addr,offset,val)    ((*((type *)(addr + offset))) = val)

// Read from a pointer plus an offset in bytes.
#define _PEEK(type,ptr,offset)          (*(type *)(((uint8_t *) ptr) + offset))

// Write to a pointer plus an offset in bytes.
#define _POKE(type,ptr,offset,val)      (*((type *)( ((uint8_t*)addr) + offset)) = val)

// Pointer to address conversions.
#define _MAKEPTR(type,addr)             ((type *)addr)
#define _TAKEPTR(type,ptr)              ((addr_t)ptr)
#define _CASTPTR(dstType,srcType,ptr)   ((dstType*)ptr)


// Other primitives -----------------------------------------------------------
extern string_t* showInt  (int i);
extern void      putStr   (string_t* str);
extern void      putStrLn (string_t* str);

