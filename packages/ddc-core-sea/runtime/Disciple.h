#ifndef _DDC_Disciple
#define _DDC_Disciple

// Minimal interface to the DDC runtime, imported by Sea Generated Modules.
//   It defines the types, macros and primops that generated Sea code uses.
//   We want to keep this API under tight control, so everything exported
//   to the generated modules is explicitly defined in this one file.
//
//   Everything should also be static-inlined, so we can run programs without
//   needing to link against external code. Primops that need manifest code
//   should be defined somewhere else.
//   
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <abort.h>


// -- Types -------------------------------------------------------------------
// An unsigned natural number.
//   Used for object sizes and field counts.
//   Big enough to represent the number of allocatable bytes.
typedef nat_t    size_t;

// A constructor tag.
typedef tag_t    uint32_t;

// A UTF8 string.
typedef string_t char*;


// -- Object Format -----------------------------------------------------------
//
//  Object: TAG2 TAG1 TAG0 FORMAT ... 
//   byte    3    2    1     0          (in MSB order)
//
//  All heap objects start with a 32-bit word containg the tag of the object,
//  and a format field in the least-significant byte.
//
//  Format Field
//  ~~~~~~~~~~~~
//
//  bit 7  6  5  4  3  2  1  0
//      -- arg ---  -- obj ---
//      X  X  X  X  X  X  0  0  -- Forward / Broken-Heart
//      X  X  X  X  a  X  X  X  -- Anchor flag
//      0  0  0  1  a  0  0  1  -- Thunk
//      0  0  1  0  a  0  0  1  -- DataBoxed
//      0  0  1  1  a  0  0  1  -- DataRaw
//      0  1  0  0  a  0  0  1  -- DataMixed
//      0  1  0  1  a  0  0  1  -- SuspIndir
//      -- size --  a  0  1  1  -- DataRawSmall
//
//  * GC Forwarding / Broken-Heart pointers.
//    During garbage collection, after the GC copies an object to the
//    "to-space" its header in the "from-space" is overwritten with a pointer
//    to where the "to-space" version of the object is.
//
//    We can identify these pointers because their lowest 2 bits are always 00.
//    This is because objects in the heap are always 4-byte aligned.
//
//    For all other values of the format field, we ensure the lowest two bits
//    are not 00.
//
//  * Anchor flag
//    If bit 3 in the format field is set then the GC is not permitted to move
//    the object. This is useful when the object has been allocated by malloc
//    and exists outside the DDC runtime's garbage collected heap.
//
//  * Data{Boxed, Mixed, Raw, RawSmall}
//    There are four data object formats:
//     DataBoxed:    A boxed object containing pointers to more heap objects.
//     DataMixed:    Some heap pointers, and some raw data.
//     DataRaw:      Contains raw data and no pointers.
//     DataRawSmall: Contains raw data where the size is small enough to 
//                   encode directly in the format field.
//
//    The -obj- (object mode) portion of the format field can be used to
//    determine if the object is a forwarding pointer, has a fixed value for
//    its format field, or is a DataRS object.
//
//
//  Note: 64-bit architectures
//  ~~~~~~~~~~~~~~~~~~~~~~~~~~~
//  The various object formats always contain an even number of 32-bit words
//  in the header portion, before the payload. This ensures that the payload
//  is 8-byte aligned, which is needed for architecures that cannot load
//  misaligned double precision floats (Float64).


// The object types.
enum _ObjType 
{       _ObjTypeUnknown,
        _ObjTypeForward,
        _ObjTypeThunk,
        _ObjTypeData,
        _ObjTypeDataR,
        _ObjTypeDataM,
        _ObjTypeSuspIndir,
        _ObjTypeDataRS
};


// Whether the object is:
//      a forwarding pointer, has a fixed format,
//      or is a DataRawSmall object that has its payload size encoded in format
//      field as well.
enum _ObjMode
{       _ObjModeForward         = 0x00,
        _ObjModeFixed           = 0x01,
        _ObjModeDataRS          = 0x03
};

// Use this mask to select the object mode portion of the format field.
#define _MaskObjMode            0x03


// If the object has a fixed format field (ie, has _ObjModeFixed)
//      then we can determine the format of the rest of the object by masking
//      the format field with the following mask and testing against this enum.
enum _ObjFixed
{       _ObjFixedThunk          = 0x11,
        _ObjFixedData           = 0x21,
        _ObjFixedDataR          = 0x31,
        _ObjFixedDataM          = 0x41,
        _ObjFixedSuspIndir      = 0x51,
        _ObjFixedMapped         = 0x71
};

#define _MaskObjFixed           0xf7


// Optional flags in the format field.
enum _ObjFlag
{        _ObjFlagAnchored        = 0x08
};

#define _MaskObjAnchored        (~_ObjFlagAnchored)


// -- Object Structures -------------------------------------------------------
// Object
// A General Object.
//   All objects contain the tag and format field as the first 32-bit word.
//   The following is a supertype of the others.
typedef struct 
{        uint32_t  tagFormat;
} Obj;


// A Boxed Data Object.
//   The payload contains pointers to other heap objects.
typedef struct 
{       uint32_t  tagFormat;    // Constructor tag and format field.
        uint32_t  arity;        // Arity of the data constructor.
                                //  (The number of pointers in the payload)
        Obj*      payload[];    
} DataBoxed;


// A Mixed Data Object.
//   The payload contains some pointers followed by raw data.
typedef struct 
{       uint32_t  tagFormat;
        uint32_t  padding;      // Padding to ensure payload is 8 byte aligned.
        uint32_t  size;         // Size of the whole object, in bytes.
        uint32_t  ptrCount;     // Number of pointers at the start of the payload.
        Obj*      payload[];    // Contains ptrCount pointers, then raw data.
} DataMixed;


// A Raw Data Object.
//   A raw data object does not contain heap pointers that need to be traced
//   by the garbage collector.
typedef struct 
{       uint32_t  tagFormat;    // Constructor tag and format field.
        uint32_t  size;         // Size of the whole object, in bytes.
        uint8_t   payload[];    // Raw data that does not contain heap pointers.
} DataRaw;


// A Small Raw object.
//   The object size is encoded as part of format field.
//    This saves us from needing to include a separate arity field.
typedef struct 
{       uint32_t  tagFormat;    // Constructor tag and format field.
        uint8_t   payload[];    // Raw data that does not contain heap pointers.
} DataRawSmall;


// -- Object Utils ------------------------------------------------------------
// Get the constructor tag of an object.
static inline 
uint32_t _tag (Obj* obj)
{       return obj ->tagFormat >> 8;
}       

// Get the format field of an object.
static inline 
uint8_t  _format (Obj* obj)
{       return (uint8_t)(obj ->tagFormat & 0x0f);
}

// Get a field of a DataRaw Object.
#define _fieldRaw(TYPE,offset,var) ((TYPE)((DataRaw*)var)->payload + offset)

// Read from a field of an Object.
//   We use an explicit macro to make it easier to see what is happening in
//   the generated code.
#define _read(ptr)                 (*ptr)

// Write to a field of an Object.
//   We use an explicit macro to make it easier to see what is happening in
//   the generated code.
#define _write(ptr,val)            ((*ptr)=(val))


// -- Error Handling ----------------------------------------------------------
// Fail ungracefully.
//   Called when we find an internal runtime error.
static inline 
void _fail(void)
{       abort();
}


// -- Storage Management ------------------------------------------------------
// Alloc some space on the heap.
//   TODO: We're just calling malloc until the GC works again.
static inline 
Obj* _alloc (size_t size)
{       return (Obj*)malloc(size);
}

// Allocate a Raw Object on the heap.
//   The payloadSize must be a multiple of the word size,
//   otherwise you'll misalign the heap.
static inline 
Obj* _allocRaw (tag_t tag, size_t payloadSizeBytes)
{       // Size of the payload in four byte words.
        size_t payloadSizeWords = payloadSizeBytes / 4;

        // Size of the whole object in bytes.
        size_t objectSizeBytes  = sizeof(DataRawSmall) + payloadSizeBytes;

        // Allocate the object.
        DataRawSmall* obj       = (DataRawSmall*)_alloc(objectSizeBytes);

        // Write the tag and format in the header.
        data ->tagFlags = (tag << 8) 
                        | _ObjModeDataRawSmall 
                        | (dataSizeWords << 4);

        return (Obj*)obj;
}


// -- Primops -----------------------------------------------------------------
// Show an integer.
//   TODO: Define in source program to allocate into a heap object.
static inline
string_t _showInt32 (int32_t i)
{       string_t str = malloc(32);
        snprintf(str, 32, "%d", i);
        return str;
}


// Print a string to stdout.
static inline
void _putStr (string_t* str)
{       fputs(stdout, str);
}

#endif

