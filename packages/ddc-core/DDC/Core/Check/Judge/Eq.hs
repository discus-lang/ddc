
module DDC.Core.Check.Judge.Eq
        (makeEq)
where
import DDC.Core.Check.Base
import DDC.Type.Transform.Crush
import qualified DDC.Type.Env   as Env


-- | Make two types equivalent to each other,
--   or throw the provided error if this is not possible.
makeEq  :: (Eq n, Ord n, Pretty n)
        => Config n
        -> a
        -> Context n
        -> Type n
        -> Type n
        -> Error a n
        -> CheckM a n (Context n)

makeEq config a ctx0 tL tR err

 -- EqLSolve
 | Just iL <- takeExists tL
 , not $ isTExists tR
 = do   let Just ctx1   = updateExists [] iL tR ctx0

        ctrace  $ vcat
                [ text "**  EqLSolve"
                , text "    LEFT:  " <> ppr tL
                , text "    RIGHT: " <> ppr tR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , empty ]

        return ctx1


 -- EqRSolve
 | Just iR <- takeExists tR
 , not $ isTExists tL
 = do   let Just ctx1   = updateExists [] iR tL ctx0

        ctrace  $ vcat
                [ text "**  EqRSolve"
                , text "    LEFT:  " <> ppr tL
                , text "    RIGHT: " <> ppr tR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , empty ]

        return ctx1


 -- EqLReach
 --  Both types are existentials, and the left is bound earlier in the stack.
 --  CAREFUL: The returned location is relative to the top of the stack,
 --           hence we need lL > lR here.
 | Just iL <- takeExists tL,    Just lL <- locationOfExists iL ctx0
 , Just iR <- takeExists tR,    Just lR <- locationOfExists iR ctx0
 , lL > lR
 = do   let Just ctx1   = updateExists [] iR tL ctx0

        ctrace  $ vcat
                [ text "**  EqLReach"
                , text "    LEFT:  " <> ppr tL
                , text "    RIGHT: " <> ppr tR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , empty ]

        return ctx1

 -- EqRReach
 --  Both types are existentials, and the right is bound earlier in the stack.
 --  CAREFUL: The returned location is relative to the top of the stack,
 --           hence we need lR > lL here.
 | Just iL <- takeExists tL,    Just lL <- locationOfExists iL ctx0
 , Just iR <- takeExists tR,    Just lR <- locationOfExists iR ctx0
 , lR > lL
 = do   let Just ctx1   = updateExists [] iL tR ctx0

        ctrace  $ vcat
                [ text "**  EqRReach"
                , text "    LEFT:  " <> ppr tL
                , text "    RIGHT: " <> ppr tR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , empty ]

        return ctx1


 -- EqVar
 | TVar u1      <- tL
 , TVar u2      <- tR
 , u1 == u2
 = do
        ctrace  $ vcat
                [ text "**  EqVar"
                , text "    LEFT:  " <> ppr tL
                , text "    RIGHT: " <> ppr tR
                , indent 4 $ ppr ctx0
                , empty ]

        return ctx0


 -- EqCon
 | TCon tc1     <- tL
 , TCon tc2     <- tR
 , equivTyCon tc1 tc2
 = do
        ctrace  $ vcat
                [ text "**  EqCon"
                , text "    LEFT:  " <> ppr tL
                , text "    RIGHT: " <> ppr tR
                , indent 4 $ ppr ctx0
                , empty ]

        return ctx0


 -- EqApp
 | TApp tL1 tL2 <- tL
 , TApp tR1 tR2 <- tR
 = do
        ctrace  $ vcat
                [ text "*>  EqApp" 
                , empty ]

        ctx1    <- makeEq config a ctx0 tL1 tR1 err
        tL2'    <- applyContext ctx1 tL2
        tR2'    <- applyContext ctx1 tR2
        ctx2    <- makeEq config a ctx1 tL2' tR2' err

        ctrace  $ vcat
                [ text "*<  EqApp"
                , text "    LEFT:   " <> ppr tL
                , text "    RIGHT:  " <> ppr tR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx2
                , empty ]

        return ctx2


 -- EqEquiv
 | equivT tL tR
 = do   ctrace  $ vcat
                [ text "**  EqEquiv" ]

        return ctx0


 -- Error
 | otherwise
 = do   ctrace  $ vcat
                [ text "DDC.Core.Check.Exp.Inst.makeEq: no match"
                , text "  LEFT:   " <> ppr tL
                , text "  RIGHT:  " <> ppr tR
                , text "  LEFTC:  " <> (ppr $ crushSomeT Env.empty tL)
                , text "  RIGHTC: " <> (ppr $ crushSomeT Env.empty tR)
                , indent 2 $ ppr ctx0 ]

        throw err

