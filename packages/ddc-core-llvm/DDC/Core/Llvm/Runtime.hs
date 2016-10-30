
module DDC.Core.Llvm.Runtime
        ( -- * GC Root chain
          nameGlobalLlvmRootChain,      varGlobalLlvmRootChain

          -- * Front Heap
        , nameGlobalHeapBase,           varGlobalHeapBase
        , nameGlobalHeapTop,            varGlobalHeapTop
        , nameGlobalHeapMax,            varGlobalHeapMax

          -- * Back Heap
        , nameGlobalHeapBackBase,       varGlobalHeapBackBase
        , nameGlobalHeapBackTop,        varGlobalHeapBackTop
        , nameGlobalHeapBackMax,        varGlobalHeapBackMax

          -- * Intrinsics
        , nameGlobalMalloc
        , nameGlobalMemcpy
        , nameGlobalGcroot)
where
import DDC.Llvm.Syntax
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Salt.Platform


-- Root Stack ------------------------------------------------------------------
-- | Name of the GC root chain provided by the LLVM runtime system.
nameGlobalLlvmRootChain :: Name
nameGlobalLlvmRootChain = NameGlobal "llvm_gc_root_chain"


-- | Make the variable that points to the GC root chain.
varGlobalLlvmRootChain :: Platform -> Var
varGlobalLlvmRootChain _pp = Var nameGlobalLlvmRootChain (TPointer (TPointer (TInt 8)))


-- Front Heap ------------------------------------------------------------------
-- | Name of the global variable that points to the first byte that can
--   be allocated on the front heap.
nameGlobalHeapBase :: Name
nameGlobalHeapBase = NameGlobal "_DDC__heapBase"

-- | Make the variable that points to the first byte that can be allocated on
--   the front heap.
varGlobalHeapBase :: Platform -> Var
varGlobalHeapBase pp = Var nameGlobalHeapBase (TPointer (tAddr pp))


-- | Name of the global variable that points to the next byte that can
--   be allocated on the front heap.
nameGlobalHeapTop :: Name
nameGlobalHeapTop = NameGlobal "_DDC__heapTop"

-- | Make the variable that points to the next byte that can be allocated
--   on the front heap.
varGlobalHeapTop :: Platform -> Var
varGlobalHeapTop pp = Var nameGlobalHeapTop (TPointer (tAddr pp))


-- | Name of the global variable that points to the highest
--   byte that can be allocated on the front heap.
nameGlobalHeapMax :: Name
nameGlobalHeapMax = NameGlobal "_DDC__heapMax"

-- | Make the variable that points to the highest byte that can be allocated
--   on the front heap.
varGlobalHeapMax :: Platform -> Var
varGlobalHeapMax pp = Var nameGlobalHeapMax (TPointer (tAddr pp))


-- Back Heap -------------------------------------------------------------------
-- | Name of the global variable that points to the first byte that can
--   be allocated on the back heap.
nameGlobalHeapBackBase :: Name
nameGlobalHeapBackBase = NameGlobal "_DDC__heapBackBase"

-- | Make the variable that points to the first byte that can be allocated on
--   the back heap.
varGlobalHeapBackBase :: Platform -> Var
varGlobalHeapBackBase pp = Var nameGlobalHeapBackBase (TPointer (tAddr pp))


-- | Name of the global variable that points to the next byte that can
--   be allocated on the back heap.
nameGlobalHeapBackTop :: Name
nameGlobalHeapBackTop = NameGlobal "_DDC__heapBackTop"

-- | Make the variable that points to the next byte that can be allocated on
--   the back heap.
varGlobalHeapBackTop :: Platform -> Var
varGlobalHeapBackTop pp = Var nameGlobalHeapBackTop (TPointer (tAddr pp))


-- | Name of the global variable that points to the highest
--   byte that can be allocated on the back heap.
nameGlobalHeapBackMax :: Name
nameGlobalHeapBackMax = NameGlobal "_DDC__heapBackMax"

-- | Make the variable that points to the highest byte that can be allocated on
--   the back heap.
varGlobalHeapBackMax :: Platform -> Var
varGlobalHeapBackMax pp = Var nameGlobalHeapBackMax (TPointer (tAddr pp))


-- Intrinsics -----------------------------------------------------------------
-- | Name of the malloc function that is used to allocate the heap.
nameGlobalMalloc  :: Name
nameGlobalMalloc  = NameGlobal "malloc"


-- | Get the name of the memcpy instrinsic for a given platform.
nameGlobalMemcpy :: Platform -> Name
nameGlobalMemcpy pp 
 = NameGlobal ("llvm.memcpy.p0i8.p0i8.i" ++ show (8 * platformNatBytes pp))


-- | Get the name of the gcroot instrinsic.
nameGlobalGcroot :: Name
nameGlobalGcroot = NameGlobal "llvm.gcroot"

