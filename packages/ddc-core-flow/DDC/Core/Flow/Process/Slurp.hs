
module DDC.Core.Flow.Process.Slurp
        (slurpOperator)
where
import DDC.Core.Flow.Process.Operator
import DDC.Core.Flow.Prim
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Type.Pretty          ()


-- | Slurp a stream operator from a let-binding binding.
--   We use this when recovering operators from the source program.
slurpOperator 
        :: Bind Name 
        -> Exp () Name 
        -> Maybe Operator

slurpOperator bResult xx
 -- Slurp a map1# operator                       
 -- TODO: handle higher arity maps generally
 | Just ( NameOpFlow (OpFlowMap 1)
        , [ XType tRate, XType tA, XType tB
          , xWorker,     (XVar _  uStream)])
                                <- takeXPrimApps xx
 , Just ([pIn1], xBody)         <- takeXLams xWorker
 = Just $ OpMap
        { opArity               = 1
        , opRate                = tRate
        , opInputs              = [uStream]
        , opResult              = bResult
        , opTypeElems           = [tA]
        , opTypeResult          = tB
        , opWorkerParams        = [pIn1]
        , opWorkerBody          = xBody }

 -- Slurp a fold# operator.
 | Just ( NameOpFlow OpFlowFold
        , [ XType tRate, XType tAcc, XType tElem
          , xWorker,     xZero,     (XVar _ uStream)])
                                <- takeXPrimApps xx
 , Just ([pAcc, pElem], xBody)  <- takeXLams xWorker
 = Just $ OpFold
        { opRate                = tRate
        , opResult              = bResult
        , opInput               = uStream
        , opTypeAcc             = tAcc
        , opTypeElem            = tElem
        , opZero                = xZero
        , opWorkerParamAcc      = pAcc
        , opWorkerParamElem     = pElem
        , opWorkerBody          = xBody }

 | otherwise
 = Nothing
