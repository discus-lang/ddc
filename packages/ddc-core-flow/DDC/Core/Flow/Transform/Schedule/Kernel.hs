
-- | A Kernel is a process that
--    1) accumulates data into sinks, rather than allocating new values.
--    2) can be scheduled into a single loop.
--    3) may be run concurrently with other kernels.
--
module DDC.Core.Flow.Transform.Schedule.Kernel
        ( scheduleKernel
        , Fail (..)
        , Lifting (..))
where
import DDC.Core.Flow.Transform.Schedule.Nest
import DDC.Core.Flow.Transform.Schedule.Fail
import DDC.Core.Flow.Transform.Schedule.Lifting
import DDC.Core.Flow.Process
import DDC.Core.Flow.Procedure
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Exp
import DDC.Core.Flow.Prim
import Control.Monad
import Data.Maybe

import DDC.Core.Flow.Transform.Schedule.SeriesEnv
        ( rateTypeOfSeriesType
        , elemTypeOfSeriesType 
        , elemBindOfSeriesBind
        , elemBoundOfSeriesBound)


-------------------------------------------------------------------------------
-- | Schedule a process kernel into a procedure.
--
--   A kernel is a process with the following restricitions:
--    1) All input series have the same rate.
--    2) A kernel accumulates data into sinks, rather than allocating new values.
--    3) A kernel can be scheduled into a single loop.
--    
scheduleKernel :: Lifting -> Process -> Either Fail Procedure
scheduleKernel 
       lifting
       (Process { processName           = name
                , processParamTypes     = bsParamTypes
                , processParamValues    = bsParamValues
                , processOperators      = operators
                , processResultType     = tResult
                , processResultExp      = xResult })

 = do   -- Check the process returns Unit.
        when (tResult /= tUnit)
         $ Left (FailReturnTypeNotUnit tResult)

        -- Check the parameter series all have the same rate.
        tK      <- slurpRateOfParamTypes (map typeOfBind bsParamValues)

        -- Check the primary rate variable matches the rates of the series.
        (case bsParamTypes of
          []            -> Left FailNoRateParameters
          BName n k : _ 
           | k == kRate
           , TVar (UName n) == tK -> return ()
          _             -> Left FailPrimaryRateMismatch)


        -- Create the initial loop nest of the process rate.
        let bsSeries    = [ b   | b <- bsParamValues
                                , isSeriesType (typeOfBind b) ]

        -- Body expressions that take the next vec of elements from each
        -- input series.
        -- TODO: throw error if type can't be lifted.
        let c           = liftingFactor lifting
        let ssBody      = [ BodyStmt 
                                (BName (NameVarMod nS "elem") tElem_lifted)
                                (xNextC c tK tElem (XVar (UName nS)) (XVar uIndex))
                                | BName nS tS     <- bsSeries
                                , let Just tElem        = elemTypeOfSeriesType tS 
                                , let uIndex            = UIx 0 
                                , let Just tElem_lifted = liftType lifting tElem ]

        let nest0       = NestLoop 
                        { nestRate      = tK 
                        , nestStart     = []
                        , nestBody      = ssBody
                        , nestInner     = NestEmpty
                        , nestEnd       = []
                        , nestResult    = xUnit }

        nest'   <- foldM (scheduleOperator lifting) nest0 operators


        -- TODO: Add Down# constructor to types of series parameters.
        return  $ Procedure
                { procedureName         = name
                , procedureParamTypes   = bsParamTypes
                , procedureParamValues  = bsParamValues
                , procedureNest         = nest'
                , procedureResultType   = tResult
                , procedureResultExp    = xResult }


-------------------------------------------------------------------------------
---- | Schedule a single series operator into a loop nest.
scheduleOperator 
        :: Lifting
        -> Nest         -- ^ The current loop nest.
        -> Operator     -- ^ The operator to schedule.
        -> Either Fail Nest

scheduleOperator lifting nest op
 -- Map -----------------------------------------
 | OpMap{}      <- op
 = do   let tK            = opInputRate op
        let context       = ContextRate tK

        -- Bind for the result element.
        let Just bResultE =   elemBindOfSeriesBind (opResultSeries op)
                          >>= liftTypeOfBind lifting

        -- Bounds for all the input series.
        let Just usInput = sequence 
                         $ map elemBoundOfSeriesBound 
                         $ opInputSeriess op

        -- Bounds for the worker parameters, along with the lifted versions.
        let bsParam     = opWorkerParams op
        let Just bsParam_lifted  
                        = sequence $ map (liftTypeOfBind lifting) bsParam
        let liftEnv     = zip bsParam bsParam_lifted

        xWorker_lifted  <- liftWorker lifting liftEnv 
                        $  opWorkerBody op

        -- Expression to apply the inputs to the worker.
        let xBody       = foldl (\x (b, p) -> XApp (XLam b x) p)
                                (xWorker_lifted)
                                [(b, XVar u) 
                                        | b <- bsParam_lifted
                                        | u <- usInput ]

        let Just nest2  = insertBody nest context
                        $ [ BodyStmt bResultE xBody ]

        return nest2

 -- Fill ----------------------------------------
 | OpFill{}     <- op
 = do   let tK          = opInputRate op
        let context     = ContextRate tK

        let Just uInput = elemBoundOfSeriesBound 
                        $ opInputSeries op

        let Just tElem_lifted
                        = liftType lifting 
                        $ opElemType op

        let Just nest2  = insertBody nest context
                        $ [ BodyStmt (BNone tUnit)
                                     (xWriteVector 
                                        tElem_lifted 
                                        (XVar $ opTargetVector op)
                                        (XVar $ UIx 0)
                                        (XVar $ uInput)) ]

        let Just nest3  = insertEnds nest2 context
                        $ [ EndStmt  (opResultBind op)
                                     xUnit ]

        return nest3

 | otherwise
 = return nest




-------------------------------------------------------------------------------
-- | Given the type of the process parameters, 
--   yield the rate of the overall process.
slurpRateOfParamTypes :: [Type Name] -> Either Fail (Type Name)
slurpRateOfParamTypes tsParam
 = case mapMaybe rateTypeOfSeriesType tsParam of
        []                      -> Left FailNoSeriesParameters
        [tK]                    -> Right tK
        (tK : ts)
         | all (== tK) ts       -> Right tK
         | otherwise            -> Left FailMultipleRates


isSeriesType :: TypeF -> Bool
isSeriesType tt
 = case takePrimTyConApps tt of
        Just (NameTyConFlow TyConFlowSeries, [_, _]) -> True
        _                                            -> False

