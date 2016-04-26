
module DDC.Core.Check.Judge.Sub
        ( makeSub)
where
import DDC.Type.Transform.SubstituteT
import DDC.Core.Exp.Annot.AnTEC
import DDC.Core.Check.Judge.Eq
import DDC.Core.Check.Judge.Inst
import DDC.Core.Check.Base


-- | Make the left type a subtype of the right type,
--   or throw the provided error if this is not possible.
makeSub :: (Eq n, Ord n, Show n, Pretty n)
        => Config n
        -> a
        -> Context n
        -> Exp  (AnTEC a n) n
        -> Type n
        -> Type n
        -> Error a n
        -> CheckM a n
                ( Exp (AnTEC a n) n
                , Context n)

-- NOTE: The order of cases matters here.
--       For example, we do something different when both sides are
--       existentials, vs the case when only one side is an existential.
makeSub config a ctx0 xL tL tR err

 -- SubCon
 --  Both sides are the same type constructor.
 | TCon tc1     <- tL
 , TCon tc2     <- tR
 , equivTyCon tc1 tc2
 = do
        ctrace  $ vcat
                [ text "**  SubCon"
                , text "    xL: " <> ppr xL
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , indent 4 $ ppr ctx0
                , empty ]

        return (xL, ctx0)


 -- SubVar
 --  Both sides are the same (rigid) type variable.
 | TVar u1      <- tL
 , TVar u2      <- tR
 , u1 == u2
 = do
        ctrace  $ vcat
                [ text "**  SubVar"
                , text "    xL: " <> ppr xL
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , indent 4 $ ppr ctx0
                , empty ]

        return (xL, ctx0)


 -- SubExVar
 --  Both sides are the same existential.
 | Just iL <- takeExists tL
 , Just iR <- takeExists tR
 , iL == iR
 = do
        ctrace  $ vcat
                [ text "**  SubExVar"
                , text "    xL: " <> ppr xL
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , indent 4 $ ppr ctx0
                , empty ]

        return (xL, ctx0)


 -- SubEquiv
 --  Both sides are equivalent
 | equivT tL tR
 = do
        ctrace  $ vcat
                [ text "**  SubEquiv"
                , text "    xL: " <> ppr xL
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , indent 4 $ ppr ctx0
                , empty ]

        return (xL, ctx0)


 -- SubInstL
 --  Left is an existential.
 | isTExists tL
 = do   ctx1    <- makeInst config a ctx0 tR tL err

        ctrace  $ vcat
                [ text "**  SubInstL"
                , text "    xL: " <> ppr xL
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , empty ]

        return (xL, ctx1)


 -- SubInstR
 --  Right is an existential.
 | isTExists tR
 = do   ctx1    <- makeInst config a ctx0 tL tR err

        ctrace  $ vcat
                [ text "**  SubInstR"
                , text "    xL: " <> ppr xL
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , empty ]

        return (xL, ctx1)


 -- SubArr
 --  Both sides are arrow types.
 | Just (tL1, tL2)  <- takeTFun tL
 , Just (tR1, tR2)  <- takeTFun tR
 = do
        ctrace  $ vcat
                [ text "*>  SubArr"
                , empty ]

        (_, ctx1) <- makeSub config a ctx0 xL tR1 tL1 err
        tL2'      <- applyContext     ctx1 tL2
        tR2'      <- applyContext     ctx1 tR2
        (_, ctx2) <- makeSub config a ctx1 xL tL2' tR2' err

        ctrace  $ vcat
                [ text "*<  SubArr"
                , text "    xL: " <> ppr xL
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , indent 4 $ ppr ctx2
                , empty ]

        return (xL, ctx2)


 -- SubApp
 --   Both sides are type applications.
 --   Assumes non-function type constructors are invariant.
 | TApp tL1 tL2 <- tL
 , TApp tR1 tR2 <- tR
 = do
        ctrace  $ vcat
                [ text "*>  SubApp"
                , empty ]

        ctx1    <- makeEq config a ctx0 tL1 tR1 err
        tL2'    <- applyContext ctx1 tL2
        tR2'    <- applyContext ctx1 tR2
        ctx2    <- makeEq config a ctx1 tL2' tR2' err

        ctrace  $ vcat
                [ text "*<  SubApp"
                , text "    xL: " <> ppr xL
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , indent 4 $ ppr ctx2
                , empty ]

        return (xL, ctx2)


 -- SubForall
 --   Left side is a forall type.
 | TForall b t1 <- tL
 = do
        ctrace  $ vcat
                [ text "*>  SubForall"
                , empty ]

        -- Make a new existential to instantiate the quantified
        -- variable and substitute it into the body.
        iA        <- newExists (typeOfBind b)
        let tA    = typeOfExists iA
        let t1'   = substituteT b tA t1

        -- Check the new body against the right type,
        -- so that the existential we just made is instantiated
        -- to match the right.
        let (ctx1, pos1) =  markContext ctx0
        let ctx2         =  pushExists  iA ctx1

        -- Wrap the expression with a type application to cause
        -- the instantiation.
        let AnTEC _ e0 c0 _
                 = annotOfExp xL
        let aFn  = AnTEC t1' (substituteT b tA e0) (substituteT b tA c0) a
        let aArg = AnTEC (typeOfBind b) (tBot kEffect) (tBot kClosure) a
        let xL1  = XApp aFn xL (XType aArg tA)

        (xL2, ctx3) <- makeSub config a ctx2 xL1 t1' tR err

        -- Pop the existential and constraints above it back off
        -- the stack.
        let ctx4  = popToPos pos1 ctx3

        ctrace  $ vcat
                [ text "*<  SubForall"
                , text "    xL:    " <> ppr xL
                , text "    LEFT:  " <> ppr tL
                , text "    RIGHT: " <> ppr tR
                , text "    xL2:   " <> ppr xL2
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx4
                , empty ]

        return (xL2, ctx4)


 -- Error
 | otherwise
 = do   ctrace  $ vcat
                [ text "DDC.Core.Check.Exp.Inst.makeSub: no match"
                , text "  LEFT:   " <> ppr tL
                , text "  RIGHT:  " <> ppr tR ]

        throw err

