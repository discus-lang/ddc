

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
