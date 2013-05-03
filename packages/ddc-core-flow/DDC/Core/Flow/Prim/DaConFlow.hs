
module DDC.Core.Flow.Prim.DaConFlow
        ( readDaConFlow
        , typeDaConFlow)
where
import DDC.Core.Flow.Prim.TyConFlow
import DDC.Core.Flow.Prim.Base
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Base.Pretty
import Data.List
import Data.Char
import Control.DeepSeq


instance NFData DaConFlow


instance Pretty DaConFlow where
 ppr dc
  = case dc of
        DaConFlowTuple n        -> text "T" <> int n <> text "#"



-- Read a baked in data constructor.
readDaConFlow :: String -> Maybe DaConFlow
readDaConFlow str
        | Just rest     <- stripPrefix "T" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ DaConFlowTuple arity

        | otherwise
        = Nothing


-- Type -----------------------------------------------------------------------
typeDaConFlow :: DaConFlow -> Type Name
typeDaConFlow cc
 = case cc of
        DaConFlowTuple 1
         -> tForalls [kData] 
         $ \[t1] -> t1  `tFunPE` tTuple1 t1

        DaConFlowTuple 2
         -> tForalls [kData, kData] 
         $ \[t1, t2] -> t1 `tFunPE` t2 `tFunPE` tTuple2 t1 t2

        _ -> error "daConFlowTuple: not finished"

