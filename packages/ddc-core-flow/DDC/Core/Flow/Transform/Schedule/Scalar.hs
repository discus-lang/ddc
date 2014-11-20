
module DDC.Core.Flow.Transform.Schedule.Scalar
        (scheduleScalar)
where
import DDC.Core.Flow.Transform.Slurp.Context
import DDC.Core.Flow.Transform.Schedule.Nest
import DDC.Core.Flow.Transform.Schedule.Error
import DDC.Core.Flow.Transform.Schedule.Base
import DDC.Core.Flow.Procedure
import DDC.Core.Flow.Process
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Prim.OpStore
import DDC.Core.Flow.Exp


-- | Schedule a process into a procedure, producing scalar code.
scheduleScalar :: Process -> Either Error Procedure
scheduleScalar 
       (Process { processName           = name
                , processParamTypes     = bsParamTypes
                , processParamValues    = bsParamValues
                , processContext        = context })
  = do
        nest            <- scheduleContext (\r _ -> return r)
                                           (scheduleOperator context) context

        return  $ Procedure
                { procedureName         = name
                , procedureParamTypes   = bsParamTypes
                , procedureParamValues  = bsParamValues
                , procedureNest         = nest }


-------------------------------------------------------------------------------
-- | Schedule a single series operator into a loop nest.
scheduleOperator 
        :: Context      -- ^ Context of all operators
        -> Operator     -- ^ Operator to schedule.
        -> Either Error ([StmtStart], [StmtBody], [StmtEnd])

scheduleOperator ctx op

 -- Id -------------------------------------------
 | OpId{}     <- op
 = do   -- Get binders for the input elements.
        let Just bResult = elemBindOfSeriesBind   (opResultSeries op)
        let Just uInput  = elemBoundOfSeriesBound (opInputSeries  op)

        return ( [] 
               , [ BodyStmt bResult (XVar uInput) ]
               , [] )

 | OpSeriesOfRateVec{} <- op
 = do   let tK           = opInputRate    op
        let tA           = opElemType     op
        let bS           = opResultSeries op
        let uInput       = opInputRateVec op
        let Just uS      = takeSubstBoundOfBind                   bS
        let Just tP      = procTypeOfSeriesType   (typeOfBind     bS)
        let Just bResult = elemBindOfSeriesBind                   bS

        -- Convert the RateVec to a series
        let starts
                = [ StartStmt bS
                        (xSeriesOfRateVec tP tK tA (XVar uInput)) ]

        -- Body expressions that take the next element from each input series.
        let bodies
                = [ BodyStmt bResult
                        (xNext tP tK tA (XVar uS) (XVar (UIx 0))) ]

        return ( starts
               , bodies
               , [] )


 | OpSeriesOfArgument{} <- op
 = do   let tK           = opInputRate    op
        let tA           = opElemType     op
        let bS           = opResultSeries op
        let Just uS      = takeSubstBoundOfBind                   bS
        let Just tP      = procTypeOfSeriesType   (typeOfBind     bS)
        let Just bResult = elemBindOfSeriesBind                   bS

        -- Body expressions that take the next element from each input series.
        -- Could be different to RateVec above, since could be from other source?
        let bodies
                = [ BodyStmt bResult
                        (xNext tP tK tA (XVar uS) (XVar (UIx 0))) ]

        return ( []
               , bodies
               , [] )


 -- Rep -----------------------------------------
 | OpRep{}      <- op
 = do   -- Make a binder for the replicated element.
        let BName nResult _ = opResultSeries op
        let nVal        = NameVarMod nResult "val"
        let uVal        = UName nVal
        let bVal        = BName nVal (opElemType op)

        -- Get the binder for the use of it in the replicated context.
        let Just bResult = elemBindOfSeriesBind (opResultSeries op)

        -- Evaluate the expression to be replicated once, 
        -- before the main loop.
        let starts
                = [ StartStmt bVal (opInputExp op) ]

        -- Use the expression for each iteration of the loop.
        let bodies
                = [ BodyStmt bResult (XVar uVal) ]

        return (starts, bodies, [])

 -- Reps ----------------------------------------
 | OpReps{}     <- op
 = do   -- Lookup binder for the input element.
        let Just uInput  = elemBoundOfSeriesBound (opInputSeries op)

        -- Set the result to point to the input element.
        let Just bResult = elemBindOfSeriesBind   (opResultSeries op)

        let bodies
                = [ BodyStmt    bResult
                                (XVar uInput)]

        return ([], bodies, [])

 -- Indices --------------------------------------
 | OpIndices{}  <- op
 = do   
        -- In a segment context the variable ^1 is the index into
        -- the current segment.
        let Just bResult = elemBindOfSeriesBind   (opResultSeries op)

        let bodies
                = [ BodyStmt    bResult
                                (XVar (UIx 1)) ]

        return ([], bodies, [])

 -- Fill -----------------------------------------
 | OpFill{} <- op
 = do   let tK          = opInputRate op

        -- Get bound of the input element.
        let Just uInput = elemBoundOfSeriesBound (opInputSeries op)

        -- Write the current element to the vector.
        let UName nVec  = opTargetVector op
        let bodies
                = [ BodyVecWrite 
                        nVec                    -- destination vector
                        (opElemType op)         -- series elem type
                        (XVar (UIx 0))          -- index
                        (XVar uInput) ]         -- value

        -- If the length of the vector corresponds to a guarded rate then it
        -- was constructed in a filter context. After the process completes, 
        -- we know how many elements were written so we can truncate the
        -- vector down to its final length.
        let ends
                | contextContainsSelect ctx tK
                = [ EndVecTrunc 
                        nVec                    -- destination vector
                        (opElemType op)         -- series element type
                        tK ]                    -- rate of source series

                | otherwise
                = []

        return ([], bodies, ends)

 -- Gather ---------------------------------------
 | OpGather{} <- op
 = do   -- Bind for result element.
        let Just bResult = elemBindOfSeriesBind (opResultBind op)

        -- Bound of source index.
        let Just uIndex  = elemBoundOfSeriesBound (opSourceIndices op)
        let buf          = xBufOfRateVec (opVectorRate op) (opElemType op)
                                         (XVar $ opSourceVector op)

        -- Read from the vector.
        let bodies      = [ BodyStmt bResult
                                (xReadVector 
                                        (opElemType op)
                                        buf
                                        (XVar $ uIndex)) ]

        return ([], bodies, [])
 
 -- Scatter --------------------------------------
 | OpScatter{} <- op
 = do   -- Bound of source index.
        let Just uIndex = elemBoundOfSeriesBound (opSourceIndices op)

        -- Bound of source elements.
        let Just uElem  = elemBoundOfSeriesBound (opSourceElems op)

        -- Read from vector.
        let bodies      = [ BodyStmt (BNone tUnit)
                                (xWriteVector
                                        (opElemType op)
                                        (XVar $ bufOfVectorName $ opTargetVector op)
                                        (XVar $ uIndex) (XVar $ uElem)) ]

        -- Bind final unit value.
        let ends        = [ EndStmt     (opResultBind op)
                                        xUnit ]

        return ([], bodies, ends)

 -- Maps -----------------------------------------
 | OpMap{} <- op
 = do   -- Bind for the result element.
        let Just bResult = elemBindOfSeriesBind (opResultSeries op)

        -- Binds for all the input elements.
        let Just usInput = sequence
                         $ map elemBoundOfSeriesBound
                         $ opInputSeriess op

        -- Apply input element vars into the worker body.
        let xBody       
                = foldl (\x (b, p) -> XApp (XLam b x) p)
                        (opWorkerBody op)
                        [(b, XVar u)
                                | b <- opWorkerParams op
                                | u <- usInput ]

        let bodies
                = [ BodyStmt bResult xBody ]

        return ([], bodies, [])

 -- Pack ----------------------------------------
 | OpPack{}     <- op
 = do   -- Lookup binder for the input element.
        let Just uInput  = elemBoundOfSeriesBound (opInputSeries op)

        -- Set the result to point to the input element
        let Just bResult = elemBindOfSeriesBind  (opResultSeries op)

        let bodies
                = [ BodyStmt    bResult
                                (XVar uInput)]

        return ([], bodies, [])

 -- Generate -------------------------------------
 | OpGenerate{} <- op
 = do   -- Bind for the result element.
        let Just bResult = elemBindOfSeriesBind (opResultSeries op)

        -- Apply loop index into the worker body.
        let xBody
                = XApp   ( XLam (opWorkerParamIndex op)
                                (opWorkerBody       op))
                         (XVar (UIx 0))          -- index

        let bodies
                = [ BodyStmt bResult xBody ]

        return ([], bodies, [])

-- Reduce --------------------------------------
 | OpReduce{} <- op
 = do   -- Initialize the accumulator.
        let UName nResult = opTargetRef op
        let nAcc          = NameVarMod nResult "acc"
        let tAcc          = typeOfBind (opWorkerParamAcc op)

        let nAccInit      = NameVarMod nResult "init"

        let starts
                = [ StartStmt (BName nAccInit tAcc)
                              (xRead tAcc (XVar $ opTargetRef op))
                  , StartAcc   nAcc tAcc (XVar (UName nAccInit)) ]

        -- Lookup binders for the input elements.
        let Just uInput = elemBoundOfSeriesBound (opInputSeries op)
        
        -- Bind for intermediate accumulator value.
        let nAccVal     = NameVarMod nResult "val"
        let uAccVal     = UName nAccVal
        let bAccVal     = BName nAccVal tAcc

        -- Substitute input and accumulator vars into worker body.
        let xBody x1 x2
                = XApp  (XApp   ( XLam (opWorkerParamAcc   op)
                                      $ XLam (opWorkerParamElem  op)
                                             (opWorkerBody op))
                                x1)
                        x2
                       
        -- Update the accumulator in the loop body.
        let bodies
                = [ BodyAccRead  nAcc tAcc bAccVal
                  , BodyAccWrite nAcc tAcc 
                        (xBody  (XVar uAccVal) 
                                (XVar uInput)) ]
                                
        -- Read back the final value after the loop has finished and
        -- write it to the destination.
        let nAccRes     = NameVarMod nResult "res"
        let ends
                = [ EndAcc   nAccRes tAcc nAcc 
                  , EndStmt  (BNone tUnit)
                             (xWrite tAcc (XVar $ opTargetRef op)
                                          (XVar $ UName nAccRes)) ]

        return (starts, bodies, ends)

 -- Unsupported ----------------------------------
 | otherwise
 = Left $ ErrorUnsupported op

