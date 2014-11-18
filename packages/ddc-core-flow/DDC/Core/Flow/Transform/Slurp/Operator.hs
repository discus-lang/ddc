
module DDC.Core.Flow.Transform.Slurp.Operator
        ( slurpOperator
        , isSeriesOperator
        , isVectorOperator)
where
import DDC.Core.Flow.Process.Operator
import DDC.Core.Flow.Exp
import DDC.Core.Flow.Prim
import DDC.Core.Compounds.Simple
import DDC.Core.Pretty                  ()
import Control.Monad


-- | Slurp a stream operator from a let-binding binding.
--   We use this when recovering operators from the source program.
slurpOperator 
        :: Bind Name 
        -> Exp () Name 
        -> Maybe ([Name], Type Name, Operator)
        -- name of contexts to insert into, rate, operator

slurpOperator bResult xx
 
 -- Rep -----------------------------------------
 | Just ( NameOpSeries OpSeriesRep
        , [ XType _P, XType tK1, XType tA, xVal@(XVar (UName nVal))])
                                <- takeXPrimApps xx
 = Just ( [nVal]
        , tK1
        , OpRep
        { opResultSeries        = bResult
        , opOutputRate          = tK1
        , opElemType            = tA
        , opInputExp            = xVal } )

 -- Reps ----------------------------------------
 | Just ( NameOpSeries OpSeriesReps
        , [ XType _P, XType tK1, XType tK2, XType tA, XVar uSegd@(UName nSegd), XVar uS@(UName nS) ])
                                <- takeXPrimApps xx
 = Just ( [nS, nSegd]
        , tK2
        , OpReps
        { opResultSeries        = bResult
        , opInputRate           = tK1
        , opOutputRate          = tK2
        , opElemType            = tA
        , opSegdBound           = uSegd
        , opInputSeries         = uS } )

 -- Indices -------------------------------------
 | Just ( NameOpSeries OpSeriesIndices
        , [ XType _P, XType tK1, XType tK2, XVar uSegd@(UName nSegd)])
                                <- takeXPrimApps xx
 = Just ( [nSegd]
        , tK2
        , OpIndices
        { opResultSeries        = bResult
        , opInputRate           = tK1
        , opOutputRate          = tK2 
        , opSegdBound           = uSegd } )

 -- Fill ----------------------------------------
 | Just ( NameOpSeries OpSeriesFill
        , [ XType _P, XType tK, XType tA, XVar uV, XVar uS@(UName nS) ])
                                <- takeXPrimApps xx
 = Just ( [nS]
        , tK
        , OpFill
        { opResultBind          = bResult
        , opTargetVector        = uV
        , opInputRate           = tK 
        , opInputSeries         = uS
        , opElemType            = tA } )


 -- Gather --------------------------------------
 | Just ( NameOpSeries OpSeriesGather
        , [ XType _P, XType tK1, XType tK2, XType tA, XVar uV, XVar uS@(UName nS) ])
                                <- takeXPrimApps xx
 = Just ( [nS]
        , tK2
        , OpGather
        { opResultBind          = bResult
        , opSourceVector        = uV
        , opVectorRate          = tK1
        , opSourceIndices       = uS
        , opInputRate           = tK2
        , opElemType            = tA } )


 -- Scatter -------------------------------------
 | Just ( NameOpSeries OpSeriesScatter
        , [ XType _P, XType tK, XType tA, XVar uV, XVar uIndices@(UName nIndices), XVar uElems@(UName nElems) ])
                                <- takeXPrimApps xx
 = Just ( [nIndices, nElems]
        , tK
        , OpScatter
        { opResultBind          = bResult
        , opTargetVector        = uV
        , opSourceIndices       = uIndices
        , opSourceElems         = uElems
        , opInputRate           = tK
        , opElemType            = tA } )


 -- Map -----------------------------------------
 | Just (NameOpSeries (OpSeriesMap n), xs) 
                                <- takeXPrimApps xx
 , n >= 1
 , XType _P : XType tR : xsArgs2   <- xs
 , (xsA, xsArgs3)       <- splitAt (n + 1) xsArgs2
 , tsA                  <- [ t | XType t <- xsA ]
 , length tsA      == n + 1
 , xWorker : xsSeries   <- xsArgs3
 , usSeries             <- [ u  | XVar  u  <- xsSeries ]
 , nsSeries             <- [ un | UName un <- usSeries ]
 , length usSeries == n
 , Just (psIn, xBody)           <- takeXLams xWorker
 , length psIn     == n
 = Just ( nsSeries
        , tR
        , OpMap
        { opArity               = n
        , opResultSeries        = bResult
        , opInputRate           = tR
        , opInputSeriess        = usSeries
        , opWorkerParams        = psIn
        , opWorkerBody          = xBody } )


 -- Pack ----------------------------------------
 | Just ( NameOpSeries OpSeriesPack
        , [ XType _P
          , XType tRateInput, XType tRateOutput
          , XType tElem
          , XVar (UName nSel), XVar uSeries@(UName nSeries) ])    <- takeXPrimApps xx
 = Just ( [nSeries, nSel]
        , tRateOutput
        , OpPack
        { opResultSeries        = bResult
        , opInputRate           = tRateInput
        , opInputSeries         = uSeries
        , opOutputRate          = tRateOutput 
        , opElemType            = tElem } )


 -- Generate ------------------------------------
 | Just ( NameOpSeries OpSeriesGenerate
        , [ XType _P, XType tK, XType _, xWorker ])
                                <- takeXPrimApps xx
 , Just ([bIx], xBody)          <- takeXLams xWorker
 = Just ( []
        , tK
        , OpGenerate
        { opResultSeries        = bResult
        , opOutputRate          = tK
        , opWorkerParamIndex    = bIx
        , opWorkerBody          = xBody } )

 -- Reduce --------------------------------------
 | Just ( NameOpSeries OpSeriesReduce
        , [ XType _P, XType tK, XType _
          , XVar uRef, xWorker, xZ, XVar uS@(UName nS) ])
                                <- takeXPrimApps xx
 , Just ([bAcc, bElem], xBody)  <- takeXLams xWorker
 = Just ( [nS]
        , tK
        , OpReduce
        { opResultBind          = bResult
        , opTargetRef           = uRef
        , opInputRate           = tK
        , opInputSeries         = uS
        , opZero                = xZ
        , opWorkerParamAcc      = bAcc
        , opWorkerParamElem     = bElem
        , opWorkerBody          = xBody } )

 | otherwise
 = Nothing


-- | Check if some binding is a series operator.
isSeriesOperator :: Exp () Name -> Bool
isSeriesOperator xx
 = case liftM fst $ takeXPrimApps xx of
        Just (NameOpSeries _)   -> True
        _                       -> False


-- | Check if some binding is a vector operator.
isVectorOperator :: Exp () Name -> Bool
isVectorOperator xx
 = case liftM fst $ takeXPrimApps xx of
        Just (NameOpVector _)   -> True
        _                       -> False

