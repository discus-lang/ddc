
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

-- | Create loops from a list of operators.
--   TODO: allow the operators to be at different rates, 
--         and create multiple loops as needed.
scheduleProcess :: Process -> Procedure
scheduleProcess 
        (Process 
                { processName           = name
                , processParamTypes     = psType
                , processParamValues    = psValue
                , processOperators      = ops 
                , processStmts          = stmts
                , processResultType     = tResult
                , processResult         = xResult})
  =     (Procedure
                { procedureName         = name
                , procedureParamTypes   = psType
                , procedureParamValues  = psValue
                , procedureNest         = scheduleOperators [] emptySeriesEnv ops 
                , procedureStmts        = stmts
                , procedureResultType   = tResult
                , procedureResult       = xResult })


-------------------------------------------------------------------------------
-- | Schedule some series operators into a loop nest.
scheduleOperators 
        :: [Loop]       -- ^ The starting loop nest.
        -> SeriesEnv    -- ^ Series environment maps series binds to elem binds.
        -> [Operator]   -- ^ The operators to schedule.
        -> [Loop]      

scheduleOperators nest0 env ops
 = case ops of
        []              -> nest0
        op : ops'     
         -> let (env', nest')   = scheduleOperator nest0 env op
            in  scheduleOperators nest' env' ops'


-- | Schedule a single series operator into a loop nest.
scheduleOperator 
        :: [Loop]       -- ^ The current loop nest
        -> SeriesEnv    -- ^ Series environment maps series binds to elem binds.
        -> Operator     -- ^ Operator to schedule.
        -> (SeriesEnv, [Loop])

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
        BName nVec _    = opResultVector op
        context         = ContextRate (opInputRate op)
        
        nest2   = insertStarts nest1 context
                $ [ StartVecNew  
                        nVec 
                        (opElemType op) 
                        (opInputRate op) ]

        -- Insert statements that write the current element to the vector.
        nest3   = insertBody   nest2 context 
                $ [ BodyVecWrite 
                        nVec 
                        (opElemType op)
                        (XVar () (UIx 0))
                        (XVar () uInput) ]
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
        xBody   = substituteXXs 
                        (zip (opWorkerParams op) xsInputs)
                        (opWorkerBody op)

        -- Binder for a single result element in the series context.
        Just nResultSeries = takeNameOfBind $ opResultSeries op
        Just nResultElem   = elemNameOfSeriesName nResultSeries
        uResultElem        = UName nResultElem

        Just bResultElem   = elemBindOfSeriesBind (opResultSeries op)

        -- Insert the expression that computes the new result into the nest.
        context         = ContextRate $ opInputRate op
        nest2           = insertBody nest1 context
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
        tAcc    = typeOfBind (opWorkerParamAcc op)
        
        -- Insert statements that initializes the consumer.
        context = ContextRate $ opInputRate op
        nest2   = insertStarts nest1 context
                        [ StartAcc nAcc tAcc (opZero op) ]

        -- Substitute input and accumulator vars into worker body.
        xBody   = substituteXXs
                        [ (opWorkerParamElem op, XVar () uInput) ]
                        (opWorkerBody op)

        -- Insert statements that update the accumulator
        nest3   = insertBody nest2 context
                        [ BodyAccRead  nAcc tAcc (opWorkerParamAcc op)
                        , BodyAccWrite nAcc tAcc xBody ]
                                
        -- Insert statements that read back the final value.
        nest4   = insertEnds nest3 context
                        [ EndAcc   n tAcc nAcc ]
   in   (env1, nest4)

 | otherwise
 = error $ renderIndent 
 $ vcat [ text "repa-plugin: can't schedule series operator"
        , indent 8 $ ppr op ]



