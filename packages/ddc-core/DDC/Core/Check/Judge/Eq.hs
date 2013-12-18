
module DDC.Core.Check.Judge.Eq
        (makeEq)
where
import DDC.Core.Check.Base


-- | Make two types equivalent to each other,
--   or throw the provided error if this is not possible.
makeEq  :: (Eq n, Ord n, Pretty n)
        => a
        -> Error a n
        -> Context n
        -> Type n
        -> Type n
        -> CheckM a n (Context n)

makeEq a err ctx0 tL tR

 -- EqLSolve
 | Just iL <- takeExists tL
 , not $ isTExists tR
 = do   let ctx1        = updateExists [] iL tR ctx0

        ctrace  $ vcat
                [ text "* EqLSolve"
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr tR
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx1
                , empty ]

        return ctx1


 -- EqRSolve
 | Just iR <- takeExists tR
 , not $ isTExists tL
 = do   let ctx1        = updateExists [] iR tL ctx0

        ctrace  $ vcat
                [ text "* EqRSolve"
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr tR
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx1
                , empty ]

        return ctx1


 -- EqVar
 | TVar u1      <- tL
 , TVar u2      <- tR
 , u1 == u2
 = do   
        ctrace  $ vcat
                [ text "* EqVar"
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr tR
                , indent 2 $ ppr ctx0
                , empty ]

        return ctx0


 -- EqCon
 | TCon tc1     <- tL
 , TCon tc2     <- tR
 , tc1 == tc2
 = do   
        ctrace  $ vcat
                [ text "* EqCon"
                , text "  LEFT:  " <> ppr tL
                , text "  RIGHT: " <> ppr tR
                , indent 2 $ ppr ctx0
                , empty ]

        return ctx0


 -- EqApp
 | TApp tL1 tL2 <- tL
 , TApp tR1 tR2 <- tR
 = do
        ctx1     <- makeEq a err ctx0 tL1 tR1
        let tL2' = applyContext ctx1 tL2
        let tR2' = applyContext ctx1 tR2
        ctx2     <- makeEq a err ctx0 tL2' tR2'

        ctrace  $ vcat
                [ text "* EqApp"
                , text "  LEFT:   " <> ppr tL
                , text "  RIGHT:  " <> ppr tR
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx2
                , empty ]

        return ctx2

 -- Error
 | otherwise
 = do   ctrace  $ vcat
                [ text "DDC.Core.Check.Exp.Inst.makeEq: no match" 
                , text "  LEFT:   " <> ppr tL
                , text "  RIGHT:  " <> ppr tR ]

        throw err

