
module DDC.Core.Llvm.Runtime
        ( varGlobal

         -- * GC Root chain
        , nameGlobalLlvmRootChain,      varGlobalLlvmRootChain

          -- * Intrinsics
        , nameGlobalMalloc
        , nameGlobalMemcpy
        , nameGlobalMemset
        , nameGlobalGcroot)
where
import DDC.Llvm.Syntax
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Salt.Platform


-- Globals ---------------------------------------------------------------------
-- | Refer to a global variable.
varGlobal :: Platform -> String -> Var
varGlobal pp name = Var (NameGlobal name) (TPointer (tAddr pp))


-- Root Stack ------------------------------------------------------------------
-- | Name of the GC root chain provided by the LLVM runtime system.
nameGlobalLlvmRootChain :: Name
nameGlobalLlvmRootChain = NameGlobal "llvm_gc_root_chain"


-- | Make the variable that points to the GC root chain.
varGlobalLlvmRootChain :: Platform -> Var
varGlobalLlvmRootChain _pp = Var nameGlobalLlvmRootChain (TPointer (TPointer (TInt 8)))


-- Intrinsics -----------------------------------------------------------------
-- | Name of the malloc function that is used to allocate the heap.
nameGlobalMalloc  :: Name
nameGlobalMalloc  = NameGlobal "malloc"


-- | Get the name of the memcpy instrinsic for a given platform.
nameGlobalMemcpy :: Platform -> Name
nameGlobalMemcpy pp 
 = NameGlobal ("llvm.memcpy.p0i8.p0i8.i" ++ show (8 * platformNatBytes pp))


-- | Get the name of the memcpy instrinsic for a given platform.
nameGlobalMemset :: Platform -> Name
nameGlobalMemset pp 
 = NameGlobal ("llvm.memset.p0i8.i" ++ show (8 * platformNatBytes pp))


-- | Get the name of the gcroot instrinsic.
nameGlobalGcroot :: Name
nameGlobalGcroot = NameGlobal "llvm.gcroot"

