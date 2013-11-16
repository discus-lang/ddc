
module DDC.Core.Check.Exp.Inst
        (checkSub)
where
import DDC.Core.Check.Exp.Base

-- Sub ------------------------------------------------------------------------
-- This is the subtyping rule for the type checking judgment.
checkSub table !a ctx0 xx tExpect
 = do
        (xx', tSynth, eff, clo, ctx1)
         <- tableCheckExp table table ctx0 xx Synth

        -- Substitute context into synthesised and expected types.
        let tExpect'    = applyContext ctx1 tExpect
        let tSynth'     = applyContext ctx1 tSynth

        ctx2    <- makeSub table a ctx1 tSynth' tExpect'

        ctrace  $ vcat
                [ text "* Sub"
                , indent 2 $ ppr xx
                , text "  tExpect:  " <> ppr tExpect
                , text "  tSynth:   " <> ppr tSynth
                , text "  tExpect': " <> ppr tExpect'
                , text "  tSynth':  " <> ppr tSynth'
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx1
                , indent 2 $ ppr ctx2 
                , empty ]

        returnX a
                (\_ -> xx')
                tExpect
                eff clo ctx2


-- makeSub --------------------------------------------------------------------
-- Make one type a subtype of another.
makeSub table a ctx0 tL tR

 -- SubExvar
 | Just iL <- takeExists tL
 , Just iR <- takeExists tR
 , iL == iR
 = do   
        ctrace  $ vcat
                [ text "* SubExVar"
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr tR
                , indent 2 $ ppr ctx0 
                , empty ]

        return ctx0

 -- SubInstantiateR
 --  TODO: do free variables check  tR /= FV(tL)
 | isTExists tR
 = do

        ctx1    <- inst table a ctx0 tL tR

        ctrace  $ vcat
                [ text "* SubInstantiateR"
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr tR
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx1 
                , empty ]

        return ctx1

 | otherwise
 = error $ renderIndent $ vcat
        [ text "subInstR: no match" 
        , text "tL = " <> ppr tL
        , text "tR = " <> ppr tR ]


-- Inst ----------------------------------------------------------------------
inst table !a ctx0 tL tR

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
        iL1      <- newExists
        let tL1  =  typeOfExists iL1 

        iL2      <- newExists
        let tL2  =  typeOfExists iL2

        -- Update the context with the new constraint.
        let ctx1 = updateExists [iL2, iL1] iL (tFun tL1 tL2) ctx0

        -- Instantiate the parameter type.
        ctx2     <- inst table a ctx1 tR1 tL1

        -- Substitute into tR2
        let tR2' =  applyContext ctx2 tR2

        -- Instantiate the return type.
        ctx3     <- inst table a ctx2 tL2 tR2'

        ctrace  $ vcat
                [ text "* InstLArr"
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr tR
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx3 
                , empty ]

        return ctx3


 -- InstRArr
 --  Left is an function arrow, and right is an existential.
 | Just (tL1, tL2)      <- takeTFun tL
 , Just iR              <- takeExists tR
 = do   
        -- Make new existentials to match the function type and parameter.
        iR1      <- newExists
        let tR1  =  typeOfExists iR1 

        iR2      <- newExists
        let tR2  =  typeOfExists iR2

        -- Update the context with the new constraint.
        let ctx1 =  updateExists [iR2, iR1] iR (tFun tR1 tR2) ctx0

        -- Instantiate the parameter type.
        ctx2     <- inst table a ctx1 tR1 tL1

        -- Substitute into tL2
        let tL2' = applyContext ctx2 tL2

        -- Instantiate the return type.
        ctx3     <- inst table a ctx2 tL2' tR2 

        ctrace  $ vcat
                [ text "* InstRArr"
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr tR
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx3 
                , empty ]

        return ctx3

 | otherwise
 = do
        ctrace  $ vcat
                [ text "* BROKEN"
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr tR
                , indent 2 $ ppr ctx0
                , empty ]

        return ctx0
