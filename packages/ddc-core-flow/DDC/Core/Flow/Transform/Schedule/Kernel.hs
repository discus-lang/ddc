
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
import DDC.Core.Flow.Process
import DDC.Core.Flow.Procedure
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Exp
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Transform.Schedule.Nest
import Control.Monad
import Data.Maybe
import Data.List

import DDC.Core.Flow.Transform.Schedule.SeriesEnv
        ( rateTypeOfSeriesType
        , elemTypeOfSeriesType 
        , elemBindOfSeriesBind
        , elemBoundOfSeriesBound)

-------------------------------------------------------------------------------
-- | Reason a process kernel could not be scheduled into a procedure.
data Fail
        -- | The return type of a kernel must be Unit, but it wasn't.
        = FailReturnTypeNotUnit
        { failReturnType        :: Type Name }

        -- | Process has no rate parameters.
        | FailNoRateParameters

        -- | Process has no series parameters, 
        --   but there needs to be at least one.
        | FailNoSeriesParameters

        -- | Process has series of different rates,
        --   but all series must have the same rate.
        | FailMultipleRates

        -- | Primary rate variable of the process does not match
        --   the rate of the paramter series.
        | FailPrimaryRateMismatch
        deriving Show


-------------------------------------------------------------------------------
-- | Lifting config controls how many elements should be processed 
--   per loop iteration.
data Lifting
        = Lifting
        { -- How many elements to process for each loop iteration.
          liftingFactor         :: Int 

        , liftingOkPrimArith    :: PrimArith -> TypeF -> Bool }


liftType :: Lifting -> TypeF -> Maybe TypeF 
liftType l tt
 | liftingFactor l == 1
 = Just tt

 | Just (NamePrimTyCon (PrimTyConFloat 32), []) 
        <- takePrimTyConApps tt
 = Just (tVec (liftingFactor l) tt)

 | otherwise
 = Nothing


liftTypeOfBind :: Lifting -> BindF -> Maybe BindF
liftTypeOfBind l b
 = case b of
        BName n t       -> liftM (BName n) (liftType l t)
        BAnon   t       -> liftM BAnon     (liftType l t)
        BNone   t       -> liftM BNone     (liftType l t)


-- | Map original variable to lifted version.
type LiftEnv
        = [(BindF, BindF)]


-- | Try to lift a first-order worker expression to work on elements of vector
--   type instead of scalars.
liftWorker :: Lifting -> LiftEnv -> ExpF -> Either Fail ExpF
liftWorker lifting env xx
 = let down     = liftWorker lifting env
   in  case xx of
        XApp x1@(XVar (UPrim (NamePrimArith prim) _)) (XType tElem)
         |  liftingOkPrimArith lifting prim tElem
         ,  Just tElem_lifted <- liftType lifting tElem
         -> Right $ XApp x1 (XType tElem_lifted)

        XApp x1 x2      
         -> do  x1'     <- down x1
                x2'     <- down x2
                return  $  XApp x1' x2'

        XVar u
         | Just (_, bL) 
                    <- find (\(bS', _) -> boundMatchesBind u bS') env
         , Just uL  <- takeSubstBoundOfBind bL
         -> Right (XVar uL)

        _ -> error $ "no lift " ++ show xx


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

