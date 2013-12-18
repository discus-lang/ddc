
module DDC.Core.Check.Judge.Inst
        (makeInst)
where
import DDC.Core.Check.Base


-- | Make the left type an instantiation of the right type,
--   or throw the provided error if this is not possible.
makeInst :: (Eq n, Ord n, Pretty n)
        => a
        -> Error a n
        -> Context n
        -> Type n
        -> Type n
        -> CheckM a n (Context n)

makeInst !a !err !ctx0 !tL !tR

 -- InstLSolve
 | Just iL <- takeExists tL
 , not $ isTExists tR
 = do   let ctx1        = updateExists [] iL tR ctx0

        ctrace  $ vcat
                [ text "* InstLSolve"
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr tR
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx1
                , empty ]

        return ctx1


 -- InstLReach
 --  Both types are existentials, and the left is bound earlier in the stack.
 --  CAREFUL: The returned location is relative to the top of the stack,
 --           hence we need lL > lR here.
 | Just iL <- takeExists tL,    Just lL <- locationOfExists iL ctx0
 , Just iR <- takeExists tR,    Just lR <- locationOfExists iR ctx0
 , lL > lR
 = do   let ctx1        = updateExists [] iR tL ctx0
        
        ctrace  $ vcat 
                [ text "* InstLReach"
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr tR
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx1
                , empty ]

        return ctx1

 -- InstRReach
 --  Both types are existentials, and the right is bound earlier in the stack.
 --  CAREFUL: The returned location is relative to the top of the stack,
 --           hence we need lR > lL here.
 | Just iL <- takeExists tL,    Just lL <- locationOfExists iL ctx0
 , Just iR <- takeExists tR,    Just lR <- locationOfExists iR ctx0
 , lR > lL
 = do   let !ctx1       = updateExists [] iL tR ctx0

        ctrace  $ vcat 
                [ text "* InstRReach"
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr tR
                , text "  lL:    " <> ppr lL
                , text "  lR:    " <> ppr lR
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx1
                , empty ]

        return ctx1


 -- InstLArr
 --  Left is an existential, right is a function arrow.
 | Just iL              <- takeExists tL
 , Just (tR1, tR2)      <- takeTFun tR
 = do
        -- Make new existentials to match the function type and parameter.
        iL1      <- newExists kData
        let tL1  =  typeOfExists iL1 

        iL2      <- newExists kData
        let tL2  =  typeOfExists iL2

        -- Update the context with the new constraint.
        let ctx1 = updateExists [iL2, iL1] iL (tFun tL1 tL2) ctx0

        -- Instantiate the parameter type.
        ctx2     <- makeInst a err ctx1 tR1 tL1

        -- Substitute into tR2
        let tR2' =  applyContext ctx2 tR2

        -- Instantiate the return type.
        ctx3     <- makeInst a err ctx2 tL2 tR2'

        ctrace  $ vcat
                [ text "* InstLArr"
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr tR
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx3 
                , empty ]

        return ctx3


 -- InstRSolve
 | Just iR <- takeExists tR
 , not $ isTExists tL
 = do   let ctx1        = updateExists [] iR tL ctx0

        ctrace  $ vcat
                [ text "* InstRSolve"
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr tR
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx1
                , empty ]

        return ctx1


 -- InstRArr
 --  Left is an function arrow, and right is an existential.
 | Just (tL1, tL2)      <- takeTFun tL
 , Just iR              <- takeExists tR
 = do   
        -- Make new existentials to match the function type and parameter.
        iR1      <- newExists kData
        let tR1  =  typeOfExists iR1 

        iR2      <- newExists kData
        let tR2  =  typeOfExists iR2

        -- Update the context with the new constraint.
        let ctx1 =  updateExists [iR2, iR1] iR (tFun tR1 tR2) ctx0

        -- Instantiate the parameter type.
        ctx2     <- makeInst a err ctx1 tR1 tL1

        -- Substitute into tL2
        let tL2' = applyContext ctx2 tL2

        -- Instantiate the return type.
        ctx3     <- makeInst a err ctx2 tL2' tR2 

        ctrace  $ vcat
                [ text "* InstRArr"
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr tR
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx3 
                , empty ]

        return ctx3

 -- Error
 | otherwise
 = do
        ctrace  $ vcat
                [ text "DDC.Core.Check.Exp.Inst.inst: no match" 
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr tR
                , indent 2 $ ppr ctx0
                , empty ]

        throw err
