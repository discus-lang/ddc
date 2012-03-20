
module DDC.Core.Sea.Output.Check
        ( Error(..)
        , checkModule)
where
import DDC.Core.Sea.Output.Error
import DDC.Core.Sea.Output.Name
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Type.Check.Monad             (result, throw)
import qualified DDC.Type.Check.Monad   as G


-- | Fragment checker monad.
--   Used to manage errors.
type CheckM a = G.CheckM (Error a)



-- Fragment check a Sea module.
checkModule
        :: Module a Name -> Maybe (Error a)

checkModule mm  
 = case (result $ checkModuleM mm) of
        Left err        -> Just err
        _               -> Nothing


-- Fragment check a Sea module, 
--  in the check monad.
checkModuleM 
        :: Module a Name
        -> CheckM a ()

checkModuleM mm@ModuleCore{}
        | [LRec _bxs]   <- moduleLets mm
        = return ()

        | otherwise
        = throw $ ErrorNoTopLevelLetrec mm

