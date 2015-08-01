
module DDC.Core.Llvm.Runtime
        ( nameGlobalHeapTop,    varGlobalHeapTop
        , nameGlobalHeapMax,    varGlobalHeapMax
        , nameGlobalSlotBase,   varGlobalSlotBase
        , nameGlobalSlotTop,    varGlobalSlotTop
        , nameGlobalSlotMax,    varGlobalSlotMax
        , nameGlobalMalloc
        , nameGlobalMemcpy
        , nameGlobalGcroot)
where
import DDC.Llvm.Syntax
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Salt.Platform


-- | Name of the global variable that points to the next byte that can
--   be allocated.
nameGlobalHeapTop :: Name
nameGlobalHeapTop = NameGlobal "_DDC__heapTop"

-- | Make the variable that points to the next byte that can be allocated.
varGlobalHeapTop :: Platform -> Var
varGlobalHeapTop pp = Var nameGlobalHeapTop (TPointer (tAddr pp))


-- | Name of the global variable that points to the highest
--   byte that can be allocated.
nameGlobalHeapMax :: Name
nameGlobalHeapMax = NameGlobal "_DDC__heapMax"

-- | Make the variable that points to the highest byte that can be allocated.
varGlobalHeapMax :: Platform -> Var
varGlobalHeapMax pp = Var nameGlobalHeapMax (TPointer (tAddr pp))


-- | Name of the malloc function that is used to allocate the heap.
nameGlobalMalloc  :: Name
nameGlobalMalloc  = NameGlobal "malloc"


-- | Get the name of the memcpy instrinsic for a given platform.
nameGlobalMemcpy :: Platform -> Name
nameGlobalMemcpy pp = NameGlobal ("llvm.memcpy.p0i8.p0i8.i" ++ show (8 * platformNatBytes pp))


-- | Get the name of the gcroot instrinsic.
nameGlobalGcroot :: Name
nameGlobalGcroot = NameGlobal "llvm.gcroot"
