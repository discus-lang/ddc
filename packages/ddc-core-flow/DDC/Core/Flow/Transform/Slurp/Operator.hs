
module DDC.Core.Flow.Transform.Slurp.Operator
        (slurpOperator)
where
import DDC.Core.Flow.Process.Operator
import DDC.Core.Flow.Exp
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Prim.TyConPrim
import DDC.Core.Compounds.Simple
import DDC.Type.Pretty          ()


-- | Slurp a stream operator from a let-binding binding.
--   We use this when recovering operators from the source program.
slurpOperator 
        :: Bind Name 
        -> Exp () Name 
        -> Maybe Operator

slurpOperator bResult xx

 -- Create --------------------------------------
 | Just ( NameOpFlow OpFlowVectorOfSeries
        , [ XType tRate, XType tA, (XVar uSeries) ])
                                <- takeXPrimApps xx
 = Just $ OpCreate
        { opResultVector        = bResult
        , opInputRate           = tRate
        , opInputSeries         = uSeries 
        , opAllocRate           = Nothing
        , opElemType            = tA }

 -- Map -----------------------------------------
 -- TODO: handle higher arity maps generally
 | Just ( NameOpFlow (OpFlowMap 1)
        , [ XType tRate, XType _tA, XType _tB
          , xWorker,     (XVar uSeries)])
                                <- takeXPrimApps xx
 , Just ([pIn1], xBody)         <- takeXLams xWorker
 = Just $ OpMap
        { opArity               = 1
        , opResultSeries        = bResult
        , opInputRate           = tRate
        , opInputSeriess        = [uSeries]
        , opWorkerParams        = [pIn1]
        , opWorkerBody          = xBody }
 -- Map2
 | Just ( NameOpFlow (OpFlowMap 2)
        , [ XType tRate, XType _tA, XType _tB, XType _tC
          , xWorker
          , XVar uSeries1, XVar uSeries2])
                                <- takeXPrimApps xx
 , Just ([pIn1, pIn2], xBody)   <- takeXLams xWorker
 = Just $ OpMap
        { opArity               = 2
        , opResultSeries        = bResult
        , opInputRate           = tRate
        , opInputSeriess        = [uSeries1, uSeries2]
        , opWorkerParams        = [pIn1, pIn2]
        , opWorkerBody          = xBody }


 -- Fold ----------------------------------------
 | Just ( NameOpFlow OpFlowFold
        , [ XType tRate, XType _tAcc, XType _tElem
          , xWorker,     xZero,     (XVar uSeries)])
                                <- takeXPrimApps xx
 , Just ([pAcc, pElem], xBody)  <- takeXLams xWorker
 = Just $ OpFold
        { opResultValue         = bResult
        , opInputRate           = tRate
        , opInputSeries         = uSeries
        , opZero                = xZero
        , opWorkerParamIndex    = BNone tInt
        , opWorkerParamAcc      = pAcc
        , opWorkerParamElem     = pElem
        , opWorkerBody          = xBody }

 | Just ( NameOpFlow OpFlowFoldIndex
        , [ XType tRate, XType _tAcc, XType _tElem
          , xWorker,     xZero,     (XVar uSeries)])
                                    <- takeXPrimApps xx
 , Just ([pIx, pAcc, pElem], xBody) <- takeXLams xWorker
 = Just $ OpFold
        { opResultValue         = bResult
        , opInputRate           = tRate
        , opInputSeries         = uSeries
        , opZero                = xZero
        , opWorkerParamIndex    = pIx
        , opWorkerParamAcc      = pAcc
        , opWorkerParamElem     = pElem
        , opWorkerBody          = xBody }


 -- Pack ----------------------------------------
 | Just ( NameOpFlow OpFlowPack
        , [ XType tRateInput, XType tRateOutput, XType tElem
          , _xSel, (XVar uSeries) ])    <- takeXPrimApps xx
 = Just $ OpPack
        { opResultSeries        = bResult
        , opInputRate           = tRateInput
        , opInputSeries         = uSeries
        , opOutputRate          = tRateOutput 
        , opElemType            = tElem }

 | otherwise
 = Nothing

