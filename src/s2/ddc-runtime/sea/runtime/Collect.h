#pragma once
#include "Runtime.h"


// Code for debugging the LLVM GC shadow stack.
// This is the reference code from the LLVM docs, with some names changed.
//
// @brief The map for a single function's stack frame.  One of these is
//        compiled as constant data into the executable for each function.
//
// Storage of metadata values is elided if the %metadata parameter to
// @llvm.gcroot is null.
struct DDCPrimCollectFrameMap {
  uint32_t NumRoots;    //< Number of roots in stack frame.
  uint32_t NumMeta;     //< Number of metadata entries.  May be < NumRoots.
  const void *Meta[];   //< Metadata for each root.
};


// @brief A link in the dynamic shadow stack.  One of these is embedded in
//        the stack frame of each function on the call stack.
struct DDCPrimCollectStackEntry {
  //< Link to next stack entry (the caller's).
  struct DDCPrimCollectStackEntry *Next;

  //< Pointer to constant FrameMap.
  const struct DDCPrimCollectFrameMap *Map;

  //< Stack roots (in-place array).
  void *Roots[];
};

extern struct DDCPrimCollectStackEntry *llvm_gc_root_chain;

void    ddcPrimCollectGCRoots           (void (*Visitor)(void **Root, const void *Meta));
void    ddcPrimCollectTraceGCRoots      (int _x);
void*   ddcLlvmRootGetStart             (int _x);
nat_t   ddcLlvmRootIsEnd                (void* p);
