
-- | Loop related names.
module DDC.Core.Flow.Prim.OpLoop
        ( readOpLoop
        , typeOpLoop)
where
import DDC.Core.Flow.Prim.TyConPrim
import DDC.Core.Flow.Prim.Base
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Base.Pretty
import Control.DeepSeq


instance NFData OpLoop


instance Pretty OpLoop where
 ppr fo
  = case fo of
        OpLoopLoop              -> text "loop#"


-- | Read a baked-in loop operator.
readOpLoop :: String -> Maybe OpLoop
readOpLoop str
        | str == "loop#"
        = Just $ OpLoopLoop

        | otherwise
        = Nothing


-- Types ----------------------------------------------------------------------
typeOpLoop  :: OpLoop -> Type Name
typeOpLoop op
 = case op of
        -- loop#  :: Int -> (Int -> Unit) -> Unit
        OpLoopLoop
         -> tInt `tFunPE` (tInt `tFunPE` tUnit) `tFunPE` tUnit
