// Hooks defined in the base library that can be called back on by the runtime
// system and the definition of primitive operators.
#pragma once
#include "runtime/Types.h"

// The top-level effect handler.
//   ddcHookHandleTopLevel {@e: Effect} (comp: S e Unit): S (Console + e) Unit
extern Obj* ddcHookHandleTopLevel(Obj* comp);

// Throw a System File exception.
extern Obj* ddcHookErrorSystemFile(int errno_value, Obj* txt);

// Throw a System Network exception.
extern Obj* ddcHookErrorSystmeNetwork(int errno_value, Obj* txt);

