
module DDC.Core.Flow.Exp.Process
        ( Process       (..)
        , Operator      (..)
        , elemTypeOfOperator
        , slurpOperator)
where
import DDC.Core.Exp
import DDC.Core.Compounds
import DDC.Core.Flow.Prim


-- | A stream process consisting of abstract stream operators.
--   We get one of these for each top-level stream function in the
--   original program.
data Process
        = Process
        { processName           :: Name
        , processType           :: Type Name
        , processParamTypes     :: [Bind Name]
        , processParamValues    :: [Bind Name]
        , processOperators      :: [Operator] 
        , processResult         :: Exp () Name }


-------------------------------------------------------------------------------
-- | An abstract stream operator.
data Operator

        -- Some base-band thing that doesn't process streams.
        = OpBase
        { opExp                 :: Exp () Name }

        -- Fold all the elements of a stream.
        | OpFold
        { opRate                :: Type   Name
        , opResult              :: Bind   Name
        , opStream              :: Bound  Name

        , opTypeAcc             :: Type   Name
        , opTypeStream          :: Type   Name

        , opZero                :: Exp () Name

        , opWorkerParamAcc      :: Bind   Name
        , opWorkerParamElem     :: Bind   Name
        , opWorkerBody          :: Exp () Name }


-- | Get the type of stream element that an operator processes.
elemTypeOfOperator :: Operator -> Maybe (Type Name)
elemTypeOfOperator op
 = case op of
        OpBase{}                -> Nothing
        OpFold{}                -> Just $ opTypeStream op


-------------------------------------------------------------------------------
-- | Slurp a stream operator from a let-binding binding.
--   We use this when recovering operators from the source program.
slurpOperator 
        :: Bind Name -> Exp () Name 
        -> Maybe Operator

slurpOperator bResult xx
 -- Slurp a fold# operator.
 | Just ( NameOpFlow OpFlowFold
        , [ XType tRate, XType tAcc, XType tStream
          , xWorker,     xZero,     (XVar _ uStream)])
                                <- takeXPrimApps xx
 , Just ([pAcc, pElem], xBody)  <- takeXLams     xWorker
 = Just $ OpFold
        { opRate                = tRate
        , opResult              = bResult
        , opStream              = uStream 

        , opTypeAcc             = tAcc
        , opTypeStream          = tStream

        , opZero                = xZero

        , opWorkerParamAcc      = pAcc
        , opWorkerParamElem     = pElem
        , opWorkerBody          = xBody }

 | otherwise
 = Nothing
