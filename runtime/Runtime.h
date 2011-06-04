
// This is the top-level header file
//	All C programs produced by DDC include this header.
//	The also need Storage/Alloc.ci to get the allocator inlines.
//
// TODO: Don't export internal RTS symbols to the client module.
//       We want to split this into the "external" and "internal" visible bits.
//
#ifndef _DDC_Runtime
#define _DDC_Runtime

// C utils
#include "Util.h"

// Runtime configuration
#include "Config.h"

// Macros expanding to runtime system magic.
#include "Macro.h"

// Object types.
#include "Types.h"
#include "Object.h"

// Error handling.
#include "Error.h"

// Runtime debugging and profilng.
#include "Debug.h"

// Runtime state, including heap and GC slot stack.
#include "State.h"

// The storage managed.
#include "Storage.h"

// Context stack for handling exceptions.
#include "Context.h"

// Application functions.
#include "Apply.h"

// Primitive functions.
#include "Prim.h"

// Pointer functions.
#include "Pointer.h"

// The top level entry point.
#include "Main.h"

#endif
