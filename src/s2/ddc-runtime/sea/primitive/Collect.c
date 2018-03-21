#pragma once
#include <stdio.h>
#include "Runtime.h"


// ----------------------------------------------------------------------------
// Code for debugging the LLVM GC shadow stack.
// This is the reference code from the LLVM docs.

// @brief The map for a single function's stack frame.  One of these is
//        compiled as constant data into the executable for each function.
//
// Storage of metadata values is elided if the %metadata parameter to
// @llvm.gcroot is null.
struct FrameMap {
  uint32_t NumRoots;    //< Number of roots in stack frame.
  uint32_t NumMeta;     //< Number of metadata entries.  May be < NumRoots.
  const void *Meta[];   //< Metadata for each root.
};


// @brief A link in the dynamic shadow stack.  One of these is embedded in
//        the stack frame of each function on the call stack.
struct StackEntry {
  struct StackEntry *Next;      //< Link to next stack entry (the caller's).
  const struct FrameMap *Map;   //< Pointer to constant FrameMap.
  void *Roots[];                //< Stack roots (in-place array).
};


// @brief The head of the singly-linked list of StackEntries. Functions push
//        and pop onto this in their prologue and epilogue.
//
// Since there is only a global list, this technique is not threadsafe.
struct StackEntry *llvm_gc_root_chain;


// @brief Calls Visitor(root, meta) for each GC root on the stack.
//        root and meta are exactly the values passed to
//        @llvm.gcroot.
//
// Visitor could be a function to recursively mark live objects.  Or it
// might copy them to another heap or generation.
//
// @param Visitor A function to invoke for every GC root on the stack.
void ddcVisitGCRoots(void (*Visitor)(void **Root, const void *Meta))
{   for (struct StackEntry *R = llvm_gc_root_chain; R; R = R->Next)
    {   uint32_t i = 0;

        // For roots [0, NumMeta), the metadata pointer is in the FrameMap.
        for (uint32_t e = R->Map->NumMeta; i != e; ++i)
          Visitor(&R->Roots[i], R->Map->Meta[i]);

        // For roots [NumMeta, NumRoots), the metadata pointer is null.
        for (uint32_t e = R->Map->NumRoots; i != e; ++i)
          Visitor(&R->Roots[i], NULL);
    }
}


// Print out the structure of the LLVM shadow stack.
void ddcTraceGCRoots (int _x)
{   for (struct StackEntry *R = llvm_gc_root_chain; R; R = R->Next)
    {   uint32_t i = 0;

        printf ("map %p\n", R->Map);

        // For roots [0, NumMeta), the metadata pointer is in the FrameMap.
        for (uint32_t e = R->Map->NumMeta; i != e; ++i)
          printf ("root with meta %p\n", R->Roots[i]);

        // For roots [NumMeta, NumRoots), the metadata pointer is null.
        for (uint32_t e = R->Map->NumRoots; i != e; ++i)
          printf ("root no   meta %p\n", R->Roots[i]);
    }
}


// ----------------------------------------------------------------------------
// Get the first root in the LLVM GC shadow stack.
void*   ddcLlvmRootGetStart (int _x)
{       return llvm_gc_root_chain;
}


// Check if this is the last entry in the LLVM GC shadow stack.
nat_t   ddcLlvmRootIsEnd (void* p)
{       return (p == 0);
}

