
module DDC.Core.Flow.Transform.Schedule
        (scheduleProcess)
where
import DDC.Core.Flow.Transform.Schedule.SeriesEnv
import DDC.Core.Flow.Transform.Schedule.Nest
import DDC.Core.Flow.Procedure
import DDC.Core.Flow.Process
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Compounds
import DDC.Core.Transform.SubstituteXX
import DDC.Core.Exp
import DDC.Base.Pretty
import Control.Monad


-- | Create loops from a list of operators.
--   TODO: allow the operators to be at different rates, 
--         and create multiple loops as needed.
scheduleProcess :: Process -> Procedure
scheduleProcess 
        (Process 
                { processName           = name
                , processParamTypes     = psType
                , processParamValues    = psValue
                , processContexts       = contexts
                , processOperators      = ops 
                , processStmts          = stmts
                , processResultType     = tResult
                , processResult         = xResult})
  = let
        -- Create all the contexts, starting with an empty loop nest.
        Just nest1      = foldM insertContext NestEmpty contexts

        -- Schedule the series operators into the nest.
        nest2           = scheduleOperators nest1 emptySeriesEnv ops

    in  Procedure
                { procedureName         = name
                , procedureParamTypes   = psType
                , procedureParamValues  = psValue
                , procedureNest         = nest2
                , procedureStmts        = stmts
                , procedureResultType   = tResult
                , procedureResult       = xResult }


-------------------------------------------------------------------------------
-- | Schedule some series operators into a loop nest.
scheduleOperators 
        :: Nest         -- ^ The starting loop nest.
        -> SeriesEnv    -- ^ Series environment maps series binds to elem binds.
        -> [Operator]   -- ^ The operators to schedule.
        -> Nest

scheduleOperators nest0 env ops
 = case ops of
        [] -> nest0
        op : ops'     
           -> let (env', nest')   = scheduleOperator nest0 env op
              in  scheduleOperators nest' env' ops'


-- | Schedule a single series operator into a loop nest.
scheduleOperator 
        :: Nest         -- ^ The current loop nest
        -> SeriesEnv    -- ^ Series environment maps series binds to elem binds.
        -> Operator     -- ^ Operator to schedule.
        -> (SeriesEnv, Nest)

scheduleOperator nest0 env op

 -- Create ---------------------------------------
 | OpCreate{} <- op
 = let  
        -- Get binders for the input elements.
        Just nSeries    
         = takeNameOfBound (opInputSeries op)
        
        (uInput, env1, nest1)
         = bindNextElem nSeries 
                        (opInputRate op) (opElemType  op)
                        env nest0

        -- Insert statements that allocate the vector.
        --  We use the type-level series rate to describe the length of
        --  the vector. This will be repalced by a RateNat value during
        --  the concretization phase.
        BName nVec _    = opResultVector op
        context         = ContextRate (opInputRate op)
        
        Just nest2      = insertStarts nest1 context
                        $ [ StartVecNew  
                                nVec                    -- allocated vector
                                (opElemType op)         -- elem type
                                (opInputRate op) ]      -- series rate

        -- Insert statements that write the current element to the vector.
        Just nest3      = insertBody   nest2 context 
                        $ [ BodyVecWrite 
                                nVec                    -- destination vector
                                (opElemType op)         -- elem type
                                (XVar () (UIx 0))       -- index
                                (XVar () uInput) ]      -- value
   in   (env1, nest3)

 
 -- Maps -----------------------------------------
 | OpMap{} <- op
 = let  
        -- Get binders for the input elements.
        Just nsSeries   = sequence $ map takeNameOfBound $ opInputSeriess op
        tsRate          = repeat (opInputRate op)
        tsElem          = map typeOfBind $ opWorkerParams op

        (usInputs, env1, nest1)    
                        = bindNextElems (zip3 nsSeries tsRate tsElem) env nest0

        -- Variables for all the input elements.
        xsInputs        = map (XVar ()) usInputs

        -- Substitute input element vars into the worker body.
        xBody           = substituteXXs 
                                (zip (opWorkerParams op) xsInputs)
                                (opWorkerBody op)

        -- Binder for a single result element in the series context.
        Just nResultSeries = takeNameOfBind $ opResultSeries op
        Just nResultElem   = elemNameOfSeriesName nResultSeries
        uResultElem        = UName nResultElem

        Just bResultElem   = elemBindOfSeriesBind (opResultSeries op)

        -- Insert the expression that computes the new result into the nest.
        context         = ContextRate $ opInputRate op
        Just nest2      = insertBody nest1 context
                                [ BodyStmt bResultElem xBody ]

        -- Associate the variable for the result element with the result series.
        env2            = insertElemForSeries nResultSeries uResultElem env1

    in  (env2, nest2)

 -- Folds ---------------------------------------
 | OpFold{} <- op
 = let  
        -- Lookup binders for the input elements.
        Just nSeries    = takeNameOfBound (opInputSeries op)
        tRate           = opInputRate op
        tInputElem      = typeOfBind (opWorkerParamElem op)
        (uInput, env1, nest1)
                        = bindNextElem nSeries tRate tInputElem env nest0

        -- Make a name for the accumulator
        BName n@(NameVar strName) _ 
                        = opResultValue op
        nAcc            = NameVar $ strName ++ "__acc"
        
        -- Type of the accumulator
        tAcc            = typeOfBind (opWorkerParamAcc op)
        
        -- Insert statements that initialize the starting value
        --  of the accumulator.
        context         = ContextRate $ opInputRate op
        Just nest2      = insertStarts nest1 context
                                [ StartAcc nAcc tAcc (opZero op) ]

        -- Substitute input and accumulator vars into worker body.
        xBody           = substituteXXs
                                [ (opWorkerParamElem op, XVar () uInput) ]
                                (opWorkerBody op)

        -- Insert statements that update the accumulator
        --  into the loop body.
        Just nest3      = insertBody nest2 context
                                [ BodyAccRead  nAcc tAcc (opWorkerParamAcc op)
                                , BodyAccWrite nAcc tAcc xBody ]
                                
        -- Insert statements that read back the final value
        --  after the loop has finished.
        Just nest4      = insertEnds nest3 context
                                [ EndAcc   n tAcc nAcc ]
   in   (env1, nest4)

 -- Pack ----------------------------------------
 | OpPack{}     <- op
 = let  
        -- Lookup binder for the input element.
        Just nSeries    = takeNameOfBound (opInputSeries op)
        tRate           = opInputRate op
        tInputElem      = opElemType op
        (uInput, env1, nest1)
                        = bindNextElem nSeries tRate tInputElem env nest0

        -- Associate the variable for the result element with the result series.
        Just nResultSeries = takeNameOfBind (opResultSeries op)
        env2               = insertElemForSeries nResultSeries uInput env1

   in   (env2, nest1)

 | otherwise
 = error $ renderIndent 
 $ vcat [ text "repa-plugin.scheduleOperator: can't schedule operator"
        , indent 8 $ ppr op ]



