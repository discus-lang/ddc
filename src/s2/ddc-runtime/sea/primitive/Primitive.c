
// Primitive operations that sea code uses.
// In future we'll just import these with the FFI.
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <inttypes.h>
#include <string.h>
#include <alloca.h>
#include "Runtime.h"

typedef float   float32_t;
typedef double  float64_t;


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


// @brief The head of the singly-linked list of StackEntries.  Functions push
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
void visitGCRoots(void (*Visitor)(void **Root, const void *Meta)) {
  for (struct StackEntry *R = llvm_gc_root_chain; R; R = R->Next) {
    uint32_t i = 0;

    // For roots [0, NumMeta), the metadata pointer is in the FrameMap.
    for (uint32_t e = R->Map->NumMeta; i != e; ++i)
      Visitor(&R->Roots[i], R->Map->Meta[i]);

    // For roots [NumMeta, NumRoots), the metadata pointer is null.
    for (uint32_t e = R->Map->NumRoots; i != e; ++i)
      Visitor(&R->Roots[i], NULL);
  }
}


// Print out the structure of the LLVM shadow stack.
void traceGCRoots (int _x) {
  for (struct StackEntry *R = llvm_gc_root_chain; R; R = R->Next) {
    uint32_t i = 0;

    printf ("map %p\n", R->Map);

    // For roots [0, NumMeta), the metadata pointer is in the FrameMap.
    for (uint32_t e = R->Map->NumMeta; i != e; ++i)
      printf ("root with meta %p\n", R->Roots[i]);

    // For roots [NumMeta, NumRoots), the metadata pointer is null.
    for (uint32_t e = R->Map->NumRoots; i != e; ++i)
      printf ("root no   meta %p\n", R->Roots[i]);
  }
}


// Get the first root in the LLVM GC shadow stack.
void*   ddcLlvmRootGetStart (int _x)
{
        return llvm_gc_root_chain;
}


// Check if this is the last entry in the LLVM GC shadow stack.
nat_t   ddcLlvmRootIsEnd (void* p)
{
        return (p == 0);
}


// ----------------------------------------------------------------------------
// Abort the program due to an inexhaustive case match.
//
// When desugaring guards, if the compiler cannot determine that
// the guards are exhaustive then a call to this function is
// inserted as a default case.
//
Obj*    primErrorDefault(string_t* source, uint32_t line)
{
        fprintf ( stderr
                , "\nDDC runtime error: inexhaustive case match.\n at: %s:%" PRId32 "\n"
                , source, line);
        exit(1);

        return 0;
}


// ----------------------------------------------------------------------------
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
 Obj* primShow##typeName (typeSpec x) \
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
_DDC_MAKE_PRIM_SHOW_TYPE(Float64,float64_t, "%g",          24);


// -- Stdin -------------------------------------------------------------------
// Get a C string from stdin, up to the given length.
Obj*    primStdinGetVector (nat_t len)
{
        string_t* pBuf  = alloca (len);
        pBuf            = fgets (pBuf, len, stdin);
        if (pBuf == NULL) {
                printf("ddc-runtime.primStdinGetVector: failed\n");
                abort();
        }

        nat_t n         = strlen(pBuf);
        Obj* pObj       = ddcAllocRaw (0, 4 + n + 1);
        uint8_t* p8     = _ddcPayloadRaw(pObj);
        uint32_t* pLen  = (uint32_t*)p8;
        string_t* pStr  = (string_t*)(p8 + 4);

        memcpy(pStr, pBuf, n + 1);
        *pLen           = n;
        return pObj;
}


// -- Stdout ------------------------------------------------------------------
// Print a C string to stdout.
void primStdoutPutString (string_t* str)
{       fputs(str, stdout);
        fflush(stdout);
}

// Print a text literal to stdout.
void primStdoutPutTextLit (string_t* str)
{       fputs(str, stdout);
        fflush(stdout);
}

// Print a text vector to stdout.
void primStdoutPutVector (Obj* obj)
{       string_t* str = (string_t*) (_ddcPayloadRaw(obj) + 4);
        fputs(str, stdout);
        fflush(stdout);
}

// Flush stdout.
void primStdoutFlush (Obj* obj)
{       fflush(stdout);
}


// -- Stderr ------------------------------------------------------------------
// Print a C string to stderr.
// Use this when printing an error from the runtime system.
void primFailString(string_t* str)
{       fputs(str, stderr);
        fflush(stderr);
}


// -- File --------------------------------------------------------------------
// Read the contents of a file into a string.
Obj*    primFileRead (string_t* path)
{
        int fd          = open (path, O_RDONLY);
        if (fd == -1) {
                printf("primFileRead: cannot open %s\n", path);
                abort();
        }

        off_t lenBuf    = lseek (fd, 0, SEEK_END);
        lseek(fd, 0, SEEK_SET);

        Obj* pObj       = ddcAllocRaw (0, 4 + lenBuf + 1);
        uint8_t* p8     = _ddcPayloadRaw(pObj);
        uint32_t* pLen  = (uint32_t*)p8;
        string_t* pStr  = (string_t*)(p8 + 4);

        uint32_t  nRead = 0;
        for(;;) {
                ssize_t lenRead = read (fd, pStr, lenBuf);
                if (lenRead == 0) break;
                nRead += lenRead;
                pStr  += lenRead;
        }

        *pStr           = 0;
        *pLen           = nRead;

        close (fd);
        return pObj;
}

