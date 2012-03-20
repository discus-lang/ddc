
-- | Structures used by the DDC runtime system.
--   These definitions need to be compatible across code compile compiled via the
--   C backend and the LLVM backend. 
--   They need to be kept in sync with the ones in runtime/Disciple.h
--
module DDC.Core.Llvm.Runtime.Object
        ( sHeapObj 
        , tHeapObj )
where
import DDC.Core.Llvm.Platform
import DDC.Llvm.Type


-- | Type of Heap objects.
--   All objects have a 32bit header word out the front.
sHeapObj :: Platform -> LlvmType
sHeapObj platform
        = LMStruct [LMInt (platformHeaderWidth platform)]

tHeapObj = sHeapObj
