
module DDC.Core.Check.Judge.Sub
        ( makeSub)
where
import DDC.Core.Check.Judge.Eq
import DDC.Core.Check.Judge.Inst
import DDC.Core.Check.Base


-- makeSub --------------------------------------------------------------------
-- Make one type a subtype of another.
makeSub table a ctx0 tL tR

 -- SubCon
 | TCon tc1     <- tL
 , TCon tc2     <- tR
 , tc1 == tc2
 = do   
        ctrace  $ vcat
                [ text "* SubCon"
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr tR
                , indent 2 $ ppr ctx0
                , empty ]

        return ctx0

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

 -- SubInstL
 --  TODO: do free variablec check  tL /= FV(tR)
 | isTExists tL
 = do   ctx1    <- makeInst table a ctx0 tR tL

        ctrace  $ vcat
                [ text "* SubInstL"
                , text "  LEFT:   " <> ppr tL
                , text "  RIGHT:  " <> ppr tR
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx1
                , empty ]

        return ctx1

 -- SubInstR
 --  TODO: do free variables check  tR /= FV(tL)
 | isTExists tR
 = do   ctx1    <- makeInst table a ctx0 tL tR

        ctrace  $ vcat
                [ text "* SubInstR"
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr tR
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx1 
                , empty ]

        return ctx1

 -- SubArr
 | Just (tL1, tL2)      <- takeTFun tL
 , Just (tR1, tR2)      <- takeTFun tR
 = do   
        ctx1     <- makeSub table a ctx0 tR1 tL1
        let tL2' = applyContext ctx1 tL2
        let tR2' = applyContext ctx1 tR2
        ctx2     <- makeSub table a ctx1 tL2' tR2'

        ctrace  $ vcat
                [ text "* SubInstArr"
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr tR
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx1
                , indent 2 $ ppr ctx2 ]

        return ctx2

 -- SubApp 
 -- Assumes non-function type constructors are invariant.
 | TApp tL1 tL2 <- tL
 , TApp tR1 tR2 <- tR
 = do   
        ctx1     <- makeEq table a ctx0 tL1 tR1
        let tL2' = applyContext ctx1 tL2
        let tR2' = applyContext ctx1 tR2
        ctx2     <- makeEq table a ctx1 tL2' tR2'

        ctrace  $ vcat
                [ text "* SubApp"
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr tR
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx1
                , indent 2 $ ppr ctx2 ]

        return ctx2


 -- Error
 -- TODO: nice error message.
 | otherwise
 = error $ renderIndent $ vcat
        [ text "DDC.Core.Check.Exp.Inst.makeSub: no match" 
        , text "  LEFT:   " <> ppr tL
        , text "  RIGHT:  " <> ppr tR ]
