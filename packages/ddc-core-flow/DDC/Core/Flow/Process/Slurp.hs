
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
 -- Slurp a create# operator
 | Just ( NameOpFlow OpFlowVectorOfSeries
        , [ XType tRate, XType tA, (XVar _ uSeries) ])
                                <- takeXPrimApps xx
 = Just $ OpCreate
        { opResultVector        = bResult
        , opInputRate           = tRate
        , opInputSeries         = uSeries 
        , opElemType            = tA }

 -- Slurp a map1# operator                       
 -- TODO: handle higher arity maps generally
 | Just ( NameOpFlow (OpFlowMap 1)
        , [ XType tRate, XType _tA, XType _tB
          , xWorker,     (XVar _  uSeries)])
                                <- takeXPrimApps xx
 , Just ([pIn1], xBody)         <- takeXLams xWorker
 = Just $ OpMap
        { opArity               = 1
        , opResultSeries        = bResult
        , opInputRate           = tRate
        , opInputSeriess        = [uSeries]
        , opWorkerParams        = [pIn1]
        , opWorkerBody          = xBody }

 -- Slurp a fold# operator.
 | Just ( NameOpFlow OpFlowFold
        , [ XType tRate, XType _tAcc, XType _tElem
          , xWorker,     xZero,     (XVar _ uSeries)])
                                <- takeXPrimApps xx
 , Just ([pAcc, pElem], xBody)  <- takeXLams xWorker
 = Just $ OpFold
        { opResultValue         = bResult
        , opInputRate           = tRate
        , opInputSeries         = uSeries
        , opZero                = xZero
        , opWorkerParamAcc      = pAcc
        , opWorkerParamElem     = pElem
        , opWorkerBody          = xBody }

 | otherwise
 = Nothing

