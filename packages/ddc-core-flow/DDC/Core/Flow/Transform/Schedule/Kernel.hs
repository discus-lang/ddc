
module DDC.Core.Flow.Transform.Schedule.Kernel
        ( scheduleKernel
        , Error         (..)
        , Lifting       (..))
where
import DDC.Core.Flow.Transform.Schedule.Nest
import DDC.Core.Flow.Transform.Schedule.Error
import DDC.Core.Flow.Transform.Schedule.Lifting
import DDC.Core.Flow.Transform.Schedule.Base
import DDC.Core.Flow.Process
import DDC.Core.Flow.Procedure
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Exp
import DDC.Core.Flow.Prim


-- | Schedule a process kernel into a procedure.
--
--   A process kernel is a process with the following restricitions:
--    1) All input series have the same rate.
--    2) A kernel accumulates data into sinks, rather than allocating new values.
--    3) A kernel can be scheduled into a single loop.
--    
---  The process kernel scheduler can produce code for
--    map, reduce, fill, gather, scatter
--
--   But not
--    fold   -- use reduce instead.
--    create -- use fill instead.
--    pack   -- we don't support SIMD masks.
--
scheduleKernel :: Lifting -> Process -> Either Error Procedure
scheduleKernel 
       lifting
       (Process { processName           = name
                , processParamTypes     = bsParamTypes
                , processParamValues    = bsParamValues
                , processContext        = context })
 = do   
        -- Lower rates of RateVec parameters.
        -- We also keep a copy of the original RateVec,
        -- in case it is used by a cross or a gather.
        let bsParamValues_lowered
                = concatMap (\(BName n t) 
                        -> case lowerSeriesRate lifting t of
                            Just t'
                             -> [ BName (NameVarMod n "down") t', BName n t ]
                            Nothing
                             -> [ BName n t ])
                $ bsParamValues

        let c           = liftingFactor lifting

        let frate r _   = return $ tDown c r
        let fop   o     = scheduleOperator lifting bsParamValues o

        nest <- scheduleContext frate fop context

        return  $ Procedure
                { procedureName         = name
                , procedureParamTypes   = bsParamTypes
                , procedureParamValues  = bsParamValues_lowered
                , procedureNest         = nest }


-------------------------------------------------------------------------------
-- | Schedule a single series operator into a loop nest.
scheduleOperator 
        :: Lifting
        -> ScalarEnv
        -> Operator     -- ^ The operator to schedule.
        -> Either Error ([StmtStart], [StmtBody], [StmtEnd])

scheduleOperator lifting envScalar op
 -- Id -------------------------------------------
 | OpId{}     <- op
 = do   -- Get binders for the input elements.
        let Just bResult =   elemBindOfSeriesBind   (opResultSeries op)
                         >>= liftTypeOfBind lifting
        let Just uInput  = elemBoundOfSeriesBound (opInputSeries  op)

        return ( [] 
               , [ BodyStmt bResult (XVar uInput) ]
               , [] )

 | OpSeries{} <- op
 = do   let c            = liftingFactor lifting
        let tK           = opInputRate    op
        let tKD          = tDown c tK
        let tA           = opElemType     op
        let BName n t    = opResultSeries op
        let Just t'      = lowerSeriesRate lifting t
        let bS           = BName n t'
        let UName nInput = opInputRateVec op
        let Just uS      = takeSubstBoundOfBind                   bS
        let Just tP      = procTypeOfSeriesType   (typeOfBind     bS)
        let Just bResult =   elemBindOfSeriesBind                   bS
                         >>= liftTypeOfBind lifting

        -- Convert the RateVec to a series
        let starts
                = [ StartStmt bS
                        (xSeriesOfRateVec tP tKD tA $ XVar $ UName $ NameVarMod nInput "down")]

        -- Body expressions that take the next element from each input series.
        let bodies
                = [ BodyStmt bResult
                        (xNextC c tP tK tA (XVar uS) (XVar (UIx 0))) ]

        return ( starts
               , bodies
               , [] )

 -- Map -----------------------------------------
 | OpMap{}      <- op
 = do   -- Bind for the result element.
        let Just bResultE =   elemBindOfSeriesBind (opResultSeries op)
                          >>= liftTypeOfBind lifting

        -- Bounds for all the input series.
        let Just usInput = sequence 
                         $ map elemBoundOfSeriesBound 
                         $ opInputSeriess op

        -- Bounds for the worker parameters, along with the lifted versions.
        let bsParam     = opWorkerParams op
        bsParam_lifted  <- mapM (liftTypeOfBindM lifting) bsParam
        let envLift     = zip bsParam bsParam_lifted

        xWorker_lifted  <- liftWorker lifting envScalar envLift
                        $  opWorkerBody op

        -- Expression to apply the inputs to the worker.
        let xBody       = foldl (\x (b, p) -> XApp (XLam b x) p)
                                (xWorker_lifted)
                                [(b, XVar u) 
                                        | b <- bsParam_lifted
                                        | u <- usInput ]

        let bodies      = [ BodyStmt bResultE xBody ]

        return ([], bodies, [])

 -- Fill ----------------------------------------
 | OpFill{}     <- op
 = do   let c           = liftingFactor lifting

        -- Bound for input element.
        let Just uInput = elemBoundOfSeriesBound 
                        $ opInputSeries op

        -- Write to target vector.
        let bodies      = [ BodyStmt (BNone tUnit)
                                     (xWriteVectorC c
                                        (opElemType op)
                                        (XVar $ bufOfVectorName $ opTargetVector op)
                                        (XVar $ UIx 0)
                                        (XVar $ uInput)) ]

        -- Bind final unit value.
        let ends        = [ EndStmt  (opResultBind op)
                                     xUnit ]

        return ([], bodies, ends)

 -- Reduce --------------------------------------
 | OpReduce{}   <- op
 = do   let c           = liftingFactor lifting
        let tA          = typeOfBind $ opWorkerParamElem op

        -- Evaluate the zero value and initialize the vector accumulator.
        let UName nRef  = opTargetRef op
        let nAccZero    = NameVarMod nRef "zero"
        let bAccZero    = BName nAccZero tA
        let uAccZero    = UName nAccZero

        let nAccVec     = NameVarMod nRef "vec"
        let uAccVec     = UName nAccVec

        let starts
                = [ StartStmt   bAccZero    (opZero op)
                  , StartAcc    nAccVec
                                (tVec c tA)
                                (xvRep c tA (XVar uAccZero)) ]

        -- Bound for input element.
        let Just uInput = elemBoundOfSeriesBound 
                        $ opInputSeries op

        -- Bound for intermediate accumulator value.
        let nAccVal     = NameVarMod nRef "val"
        let uAccVal     = UName nAccVal
        let bAccVal     = BName nAccVal (tVec c tA)

        -- Lift the worker function.
        let bsParam     = [ opWorkerParamAcc op, opWorkerParamElem op ]
        bsParam_lifted  <- mapM (liftTypeOfBindM lifting) bsParam
        let envLift     = zip bsParam bsParam_lifted

        xWorker_lifted  <- liftWorker lifting envScalar envLift 
                        $  opWorkerBody op

        -- Read the current accumulator value and update it with the worker.
        let xBody_lifted x1 x2
                = XApp (XApp ( XLam (opWorkerParamAcc   op)
                             $ XLam (opWorkerParamElem  op)
                                    (xWorker_lifted))
                             x1)
                        x2

        let bodies
                = [ BodyAccRead  nAccVec (tVec c tA) bAccVal
                  , BodyAccWrite nAccVec (tVec c tA) 
                                 (xBody_lifted (XVar uAccVal) (XVar uInput)) ]

        -- Read back the vector accumulator and to a final fold over its parts.
        let nAccResult  = NameVarMod nRef "res"
        let bAccResult  = BName nAccResult (tVec c tA)
        let uAccResult  = UName nAccResult
        let bPart (i :: Int) = BName (NameVarMod nAccResult (show i)) tA
        let uPart (i :: Int) = UName (NameVarMod nAccResult (show i))

        let nAccInit    = NameVarMod nRef "init"

        let xBody x1 x2
                = XApp (XApp ( XLam (opWorkerParamAcc op)
                             $ XLam (opWorkerParamElem op)
                                    (opWorkerBody op))
                             x1)
                        x2

        let ends
                =  [ EndStmt    bAccResult
                                (xRead (tVec c tA) (XVar uAccVec))

                   , EndStmt    (BName nAccInit tA)
                                (xRead tA (XVar $ opTargetRef op)) ]

                ++ [ EndStmt    (bPart 0)
                                (xBody  (XVar $ UName nAccInit)
                                        (xvProj c 0 tA (XVar uAccResult))) ]

                ++ [ EndStmt    (bPart i)
                                (xBody  (XVar (uPart (i - 1)))
                                        (xvProj c i tA (XVar uAccResult)))
                                | i <- [1.. c - 1]]
        -- Write final value to destination.
                ++ [ EndStmt    (BNone tUnit)
                                (xWrite tA (XVar $ opTargetRef op)
                                           (XVar $ uPart (c - 1))) ]
        -- Bind final unit value.
                ++ [ EndStmt    (opResultBind op)
                                 xUnit ]


        return (starts, bodies, ends)


 -- Gather --------------------------------------
 | OpGather{}   <- op
 = do   
        let c           = liftingFactor lifting

        -- Bind for result element.
        let Just bResultE =   elemBindOfSeriesBind (opResultBind op)
                          >>= liftTypeOfBind lifting

        -- Bound of source index.
        let Just uIndex = elemBoundOfSeriesBound (opSourceIndices op)

        -- Read from vector.
        let bodies      = [ BodyStmt bResultE
                                (xvGather c 
                                        (opVectorRate    op)
                                        (opElemType      op)
                                        (XVar $ opSourceVector  op)
                                        (XVar $ uIndex)) ]

        return ([], bodies, [])

 -- Scatter -------------------------------------
 | OpScatter{}  <- op
 = do   
        let c           = liftingFactor lifting

        -- Bound of source index.
        let Just uIndex = elemBoundOfSeriesBound (opSourceIndices op)

        -- Bound of source elements.
        let Just uElem  = elemBoundOfSeriesBound (opSourceElems op)

        -- Read from vector.
        let bodies      = [ BodyStmt (BNone tUnit)
                                (xvScatter c
                                        (opElemType op)
                                        (XVar $ opTargetVector op)
                                        (XVar $ uIndex) (XVar $ uElem)) ]

        -- Bind final unit value.
        let ends        = [ EndStmt     (opResultBind op)
                                        xUnit ]

        return ([], bodies, ends)

 -- Unsupported ---------------------------------
 | otherwise
 = Left $ ErrorUnsupported op


liftTypeOfBindM :: Lifting -> Bind Name -> Either Error (Bind Name)
liftTypeOfBindM lifting b
  = case liftTypeOfBind lifting b of
     Just b' -> return b'
     _       -> Left $ ErrorCannotLiftType (typeOfBind b)

