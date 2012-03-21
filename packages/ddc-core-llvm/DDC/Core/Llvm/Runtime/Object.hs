
-- | Structures used by the DDC runtime system.
--   These definitions need to be compatible across code compile compiled via the
--   C backend and the LLVM backend. 
--   They need to be kept in sync with the ones in runtime/Disciple.h
--
module DDC.Core.Llvm.Runtime.Object
        ( tObj,          sObj,          aObj
        , tDataRawSmall, aDataRawSmall)
where
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Platform
import DDC.Llvm.Type
import qualified Data.Map as Map



-- DataRawSmall ---------------------------------------------------------------
-- | Small raw objects.
tDataRawSmall :: Platform -> Type
tDataRawSmall platform  
        = TAlias (aDataRawSmall platform)

aDataRawSmall :: Platform -> TypeAlias
aDataRawSmall platform  
 = let  Just struct     = Map.lookup "DataRawSmall" $ platformStructs platform
   in   TypeAlias "struct.DataRawSmall" 
                $ convStruct struct
