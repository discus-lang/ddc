#pragma once
#include <stdint.h>

// Boolean type.
typedef int     bool_t;

// An unsigned natural number.
//   Used for object sizes and field counts.
//   Big enough to represent the number of allocatable bytes.
typedef size_t   nat_t;

// Define int_t to make things look consistent.
typedef int      int_t;

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


// Object
// A General Object.
//   All objects contain the tag and format field as the first 32-bit word.
//   The following is a supertype of the others.
typedef struct
{        uint32_t  tagFormat;
} Obj;

