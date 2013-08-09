
module DDC.Core.Flow.Transform.Schedule.Scalar
        (scheduleScalar)
where
import DDC.Core.Flow.Transform.Schedule.Nest
import DDC.Core.Flow.Transform.Schedule.Fail
import DDC.Core.Flow.Transform.Schedule.Base
import DDC.Core.Flow.Procedure
import DDC.Core.Flow.Process
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Exp
import Control.Monad


-- | Schedule a process into a procedure, producing scalar code.
scheduleScalar :: Process -> Either Fail Procedure
scheduleScalar 
       (Process { processName           = name
                , processParamTypes     = bsParamTypes
                , processParamValues    = bsParamValues
                , processOperators      = operators
                , processContexts       = contexts
                , processResultType     = tResult
                , processResultExp      = xResult})
  = do
        -- Check the parameter series all have the same rate.
        tK      <- slurpRateOfParamTypes 
                        $ filter isSeriesType 
                        $ map typeOfBind bsParamValues

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

        -- Body expressions that take the next element from each input series.
        let ssBody      
                = [ BodyStmt bElem
                        (xNext tK tElem (XVar (UName nS)) (XVar uIndex))
                        | bS@(BName nS tS)      <- bsSeries
                        , let Just tElem        = elemTypeOfSeriesType tS 
                        , let Just bElem        = elemBindOfSeriesBind bS
                        , let uIndex            = UIx 0 ]

        -- The initial loop nest.
        let nest0       
                = NestLoop 
                { nestRate              = tK 
                , nestStart             = []
                , nestBody              = ssBody
                , nestInner             = NestEmpty
                , nestEnd               = []
                , nestResult            = xUnit }

        -- Create the nested contexts
        let Just nest1  =  foldM insertContext nest0 contexts

        -- Schedule the series operators into the nest.
        nest2           <- foldM scheduleOperator nest1 operators

        return  $ Procedure
                { procedureName         = name
                , procedureParamTypes   = bsParamTypes
                , procedureParamValues  = bsParamValues
                , procedureNest         = nest2
                , procedureResultType   = tResult
                , procedureResultExp    = xResult }


-------------------------------------------------------------------------------
-- | Schedule a single series operator into a loop nest.
scheduleOperator 
        :: Nest         -- ^ The current loop nest.
        -> Operator     -- ^ Operator to schedule.
        -> Either Fail Nest

scheduleOperator nest0 op

 -- Id -------------------------------------------
 | OpId{}     <- op
 = do   let tK          = opInputRate op
        let context     = ContextRate tK

        -- Get binders for the input elements.
        let Just bResult = elemBindOfSeriesBind   (opResultSeries op)
        let Just uInput  = elemBoundOfSeriesBound (opInputSeries  op)

        let Just nest1   
                = insertBody nest0 context
                $ [ BodyStmt bResult (XVar uInput) ]

        return nest1

-- Maps -----------------------------------------
 | OpMap{} <- op
 = do   let tK          = opInputRate op
        let context     = ContextRate tK

        -- Bind for the result element.
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

        let Just nest1  
                = insertBody nest0 context
                $ [ BodyStmt bResult xBody ]

        return nest1

 -- Pack ----------------------------------------
 | OpPack{}     <- op
 = do   -- Lookup binder for the input element.
        let Just uInput  = elemBoundOfSeriesBound (opInputSeries op)

        -- Set the result to point to the input element
        let Just bResult = elemBindOfSeriesBind  (opResultSeries op)

        let Just nest1
                = insertBody nest0 (ContextRate (opOutputRate op))
                $ [ BodyStmt    bResult
                                (XVar uInput)]

        return nest1

-- Reduce --------------------------------------
 | OpReduce{} <- op
 = do   let tK          = opInputRate op
        let context     = ContextRate tK

        -- Initialize the accumulator.
        let UName nResult = opTargetRef op
        let nAcc          = NameVarMod nResult "acc"
        let tAcc          = typeOfBind (opWorkerParamAcc op)

        let Just nest1
                = insertStarts nest0 context
                $ [ StartAcc nAcc tAcc (opZero op) ]

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
        let Just nest2
                = insertBody nest1 context
                $ [ BodyAccRead  nAcc tAcc bAccVal
                  , BodyAccWrite nAcc tAcc 
                        (xBody  (XVar uAccVal) 
                                (XVar uInput)) ]
                                
        -- Read back the final value after the loop has finished and
        -- write it to the destination.
        let nAccRes     = NameVarMod nResult "res"
        let Just nest3      
                = insertEnds nest2 context
                $ [ EndAcc   nAccRes tAcc nAcc 
                  , EndStmt  (BNone tUnit)
                             (xWrite tAcc (XVar $ opTargetRef op)
                                          (XVar $ UName nAccRes)) ]

        return nest3

-- Fold and FoldIndex --------------------------
 | OpFold{} <- op
 = do   let tK          = opInputRate op
        let context     = ContextRate tK 

        -- Initialize the accumulator.
        let BName nResult _ = opResultValue op
        let nAcc            = NameVarMod nResult "acc"
        let tAcc            = typeOfBind (opWorkerParamAcc op)

        let Just nest1
                = insertStarts nest0 context
                $ [ StartAcc nAcc tAcc (opZero op) ]

        -- Lookup binders for the input elements.
        let Just uInput = elemBoundOfSeriesBound (opInputSeries op)
        
        -- Bind for intermediate accumulator value.
        let nAccVal     = NameVarMod nResult "val"
        let uAccVal     = UName nAccVal
        let bAccVal     = BName nAccVal tAcc

        -- Substitute input and accumulator vars into worker body.
        let xBody x1 x2 x3
                = XApp (XApp  (XApp   ( XLam (opWorkerParamIndex op) 
                                      $ XLam (opWorkerParamAcc   op)
                                      $ XLam (opWorkerParamElem  op)
                                             (opWorkerBody op))
                                       x1)
                                x2)
                       x3

        -- Update the accumulator in the loop body.
        let Just nest2
                = insertBody nest1 context
                $ [ BodyAccRead  nAcc tAcc bAccVal
                  , BodyAccWrite nAcc tAcc 
                        (xBody  (XVar $ UIx 0) 
                                (XVar uAccVal) 
                                (XVar uInput)) ]
                                
        -- Read back the final value after the loop has finished
        let Just nest3      
                = insertEnds nest2 context
                $ [ EndAcc   nResult tAcc nAcc ]

        return nest3

 -- Create ---------------------------------------
 | OpCreate{} <- op
 = do   let tK          = opInputRate op
        let context     = ContextRate tK

        -- Get bound of the input element.
        let Just uInput = elemBoundOfSeriesBound (opInputSeries op)

        -- Insert statements that allocate the vector.
        --  We use the type-level series rate to describe the length of
        --  the vector. This will be repalced by a RateNat value during
        --  the concretization phase.
        let BName nVec _    = opResultVector op

        -- Rate we're using to allocate the result vector.
        --   This will be larger than the actual result series rate if we're
        --   creating a vector inside a selector context.
        let Just tRateAlloc = opAllocRate op

        let Just nest1  
                = insertStarts nest0 context
                $ [ StartVecNew  
                        nVec                    -- allocated vector
                        (opElemType op)         -- elem type
                        tRateAlloc ]            -- allocation rate

        -- Insert statements that write the current element to the vector.
        let Just nest2      
                = insertBody   nest1 context 
                $ [ BodyVecWrite 
                        nVec                    -- destination vector
                        (opElemType op)         -- elem type
                        (XVar (UIx 0))          -- index
                        (XVar uInput) ]         -- value

        -- Slice the vector to the final length.
        let Just nest3      
                = insertEnds   nest2 context 
                $ [ EndVecSlice
                        nVec                    -- destination vector
                        (opElemType op)         -- elem type
                        (opInputRate op) ]      -- index

        -- Suppress slicing if we know the input rate is the same as
        -- the ouput rate.
        let nest'   = if opInputRate op == tRateAlloc
                          then nest2
                          else nest3

        return nest'

 -- Fill -----------------------------------------
 | OpFill{} <- op
 = do   let tK          = opInputRate op
        let context     = ContextRate tK

        -- Get bound of the input element.
        let Just uInput = elemBoundOfSeriesBound (opInputSeries op)

        -- Write the current element to the vector.
        let UName nVec  = opTargetVector op
        let Just nest1      
                = insertBody   nest0 context 
                $ [ BodyVecWrite 
                        nVec                    -- destination vector
                        (opElemType op)         -- elem type
                        (XVar (UIx 0))          -- index
                        (XVar uInput) ]         -- value

        return nest1

 -- Gather ---------------------------------------
 | OpGather{} <- op
 = do   
        let tK          = opInputRate op
        let context     = ContextRate tK

        -- Bind for result element.
        let Just bResult = elemBindOfSeriesBind (opResultBind op)

        -- Bound of source index.
        let Just uIndex  = elemBoundOfSeriesBound (opSourceIndices op)

        -- Read from the vector.
        let Just nest1  = insertBody nest0 context
                        $ [ BodyStmt bResult
                                (xReadVector 
                                        (opElemType op)
                                        (XVar $ opSourceVector op)
                                        (XVar $ uIndex)) ]

        return nest1
 
 -- Scatter --------------------------------------
 | OpScatter{} <- op
 = do   
        let tK          = opInputRate op
        let context     = ContextRate tK

        -- Bound of source index.
        let Just uIndex = elemBoundOfSeriesBound (opSourceIndices op)

        -- Bound of source elements.
        let Just uElem  = elemBoundOfSeriesBound (opSourceElems op)

        -- Read from vector.
        let Just nest1  = insertBody nest0 context
                        $ [ BodyStmt (BNone tUnit)
                                (xWriteVector
                                        (opElemType op)
                                        (XVar $ opTargetVector op)
                                        (XVar $ uIndex) (XVar $ uElem)) ]

        -- Bind final unit value.
        let Just nest2  = insertEnds nest1 context
                        $ [ EndStmt     (opResultBind op)
                                        xUnit ]

        return nest2


 -- Unsupported ----------------------------------
 | otherwise
 = Left $ FailUnsupported op

